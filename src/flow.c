/* ulp -- solve the undirected labeling problem
   Copyright (C) 2006  Falk H�ffner

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

#include <alloca.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

struct neighbor {
    unsigned neighbor;
    unsigned residual;
    int flow;
};

struct vertex {
    unsigned deg;
    struct neighbor neighbors[];
};

//struct graph {
//    unsigned n;
//    struct vertex *v;
//};

static inline unsigned *res(struct vertex **g, unsigned v, unsigned w) {
    // FIXME use binary search
    for (unsigned j = 0; ; j++)
	if (g[v]->neighbors[j].neighbor == w)
	    return &g[v]->neighbors[j].residual;
}

static inline int vertex_flow(struct vertex **g, unsigned v) {
    unsigned flow = 0;
    for (unsigned i = 0; i < g[v]->deg; i++) {
	//unsigned w = g[v]->neighbors[i].neighbor;
	//unsigned r = g[v]->neighbors[i].residual;
	//unsigned cap = (r + *res(g, w, v)) / 2;
	//flow += cap - r;
	flow += g[v]->neighbors[i].flow;
    }
    return flow;
}

static void dump_graph(struct vertex **g, unsigned n) {
    for (unsigned v = 0; v < n; v++) {
	printf("%u: %d\n", v, vertex_flow(g, v));
	for (unsigned i = 0; i < g[v]->deg; i++) {
	    unsigned w = g[v]->neighbors[i].neighbor;
	    printf("%u -> %u: %d/%u\n",
		   v, w, g[v]->neighbors[i].flow, g[v]->neighbors[i].residual);
	}
    }
}

static inline void push(struct vertex **g, unsigned v, unsigned w) {
    for (unsigned j = 0; ; j++) {
	//fprintf(stderr, "v %d:%d\n", v, g[v]->neighbors[j].neighbor);
	if (g[v]->neighbors[j].neighbor == w) {
	    --g[v]->neighbors[j].residual;
	    ++g[v]->neighbors[j].flow;
	    break;
	}
    }
    for (unsigned j = 0; ; j++) {
	//fprintf(stderr, "w %d:%d\n", w, g[w]->neighbors[j].neighbor);
	if (g[w]->neighbors[j].neighbor == v) {
	    ++g[w]->neighbors[j].residual;
	    --g[w]->neighbors[j].flow;
	    break;
	}
    }
}

static unsigned augment_many_many(struct vertex **g, unsigned n,
				  const unsigned *s, unsigned n_s, const bool *is_t) {
    //fprintf(stderr, "n =  %d\n", n);
    unsigned q[n];
    memcpy(q, s, n_s * sizeof *s);
    unsigned *qhead = q, *qtail = q + n_s;
    int pred[n];
    memset(pred, -1, sizeof pred);
    for (unsigned i = 0; i < n_s; i++)
	pred[s[i]] = s[i];
    while (qhead != qtail) {
	unsigned v = *qhead++;
	//fprintf(stderr, "popped %d\n", v);
	for (unsigned i = 0; i < g[v]->deg; i++) {
	    unsigned w = g[v]->neighbors[i].neighbor;	    
	    //fprintf(stderr, "found %d pred[%d] = %d\n", w, w, pred[w]);
	    if (pred[w] == -1 && g[v]->neighbors[i].residual > 0) {
		//fprintf(stderr, "push %d\n", w);
		pred[w] = v;
		if (is_t[w]) {
		    //fprintf(stderr, "success\n");
		    unsigned target = w;
		    while ((unsigned) pred[w] != w) {
			unsigned v = pred[w];
			//fprintf(stderr, "v = %d w = %d\n", v, w);
			push(g, v, w);
			w = v;
		    }
		    return target;
		}
		*qtail++ = w;
	    }
	}
    }

    //fprintf(stderr, "fail\n");
    return -1;
}

static unsigned augment_one_many(struct vertex **g, unsigned n,
				 const unsigned s, const bool *is_t) {
    //fprintf(stderr, "n =  %d\n", n);
    unsigned q[n];
    unsigned *qhead = q, *qtail = q;
    *qtail++ = s;
    int pred[n];
    memset(pred, -1, sizeof pred);
    pred[s] = s;
    while (qhead != qtail) {
	unsigned v = *qhead++;
	//fprintf(stderr, "popped %d\n", v);
	for (unsigned i = 0; i < g[v]->deg; i++) {
	    unsigned w = g[v]->neighbors[i].neighbor;	    
	    //fprintf(stderr, "found %d pred[%d] = %d\n", w, w, pred[w]);
	    if (pred[w] == -1 && g[v]->neighbors[i].residual > 0) {
		//fprintf(stderr, "push %d\n", w);
		pred[w] = v;
		if (is_t[w]) {
		    unsigned target = w;
		    while ((unsigned) pred[w] != w) {
			unsigned v = pred[w];
			push(g, v, w);
			w = v;
		    }
		    //fprintf(stderr, "success\n");
		    return target;
		}
		*qtail++ = w;
	    }
	}
    }

    //fprintf(stderr, "fail\n");
    return -1;
}

static unsigned drain_source(struct vertex **g, unsigned n,
			     const unsigned s, const bool *is_t) {
    //fprintf(stderr, "n =  %d\n", n);
    unsigned q[n];
    unsigned *qhead = q, *qtail = q;
    *qtail++ = s;
    int pred[n];
    memset(pred, -1, sizeof pred);
    pred[s] = s;
    while (qhead != qtail) {
	unsigned v = *qhead++;
	//fprintf(stderr, "popped %d\n", v);
	for (unsigned i = 0; i < g[v]->deg; i++) {
	    unsigned w = g[v]->neighbors[i].neighbor;	    
	    //fprintf(stderr, "found %d pred[%d] = %d\n", w, w, pred[w]);
	    if (pred[w] == -1 && g[v]->neighbors[i].flow > 0) {
		//fprintf(stderr, "push %d\n", w);
		pred[w] = v;
		if (is_t[w]) {
		    unsigned target = w;
		    while ((unsigned) pred[w] != w) {
			unsigned v = pred[w];
			push(g, w, v);
			w = v;
		    }
		    //fprintf(stderr, "success\n");
		    return target;
		}
		*qtail++ = w;
	    }
	}
    }

    //fprintf(stderr, "fail\n");
    return -1;
}

static unsigned drain_target(struct vertex **g, unsigned n,
			     const unsigned s, const bool *is_t) {
    //fprintf(stderr, "n =  %d\n", n);
    unsigned q[n];
    unsigned *qhead = q, *qtail = q;
    *qtail++ = s;
    int pred[n];
    memset(pred, -1, sizeof pred);
    pred[s] = s;
    while (qhead != qtail) {
	unsigned v = *qhead++;
	//fprintf(stderr, "popped %d\n", v);
	for (unsigned i = 0; i < g[v]->deg; i++) {
	    unsigned w = g[v]->neighbors[i].neighbor;	    
	    //fprintf(stderr, "found %d pred[%d] = %d\n", w, w, pred[w]);
	    if (pred[w] == -1 && g[v]->neighbors[i].flow < 0) {
		//fprintf(stderr, "push %d\n", w);
		pred[w] = v;
		if (is_t[w]) {
		    unsigned target = w;
		    while ((unsigned) pred[w] != w) {
			unsigned v = pred[w];
			push(g, v, w);
			w = v;
		    }
		    //fprintf(stderr, "success\n");
		    return target;
		}
		*qtail++ = w;
	    }
	}
    }

    //fprintf(stderr, "fail\n");
    return -1;
}


static unsigned augment_one_one(struct vertex **g, unsigned n,
				const unsigned s, const unsigned t) {
    //fprintf(stderr, "n =  %d\n", n);
    unsigned q[n];
    unsigned *qhead = q, *qtail = q;
    *qtail++ = s;
    int pred[n];
    memset(pred, -1, sizeof pred);
    pred[s] = s;
    while (qhead != qtail) {
	unsigned v = *qhead++;
	//fprintf(stderr, "popped %d\n", v);
	for (unsigned i = 0; i < g[v]->deg; i++) {
	    unsigned w = g[v]->neighbors[i].neighbor;	    
	    //fprintf(stderr, "found %d pred[%d] = %d\n", w, w, pred[w]);
	    if (pred[w] == -1 && g[v]->neighbors[i].residual > 0) {
		//fprintf(stderr, "push %d\n", w);
		pred[w] = v;
		if (w == t) {
		    unsigned target = w;
		    while ((unsigned) pred[w] != w) {
			unsigned v = pred[w];
			push(g, v, w);
			w = v;
		    }
		    //fprintf(stderr, "success\n");
		    return target;
		}
		*qtail++ = w;
	    }
	}
    }

    //fprintf(stderr, "fail\n");
    return -1;
}

static inline uint64_t gray_code(uint64_t x) { return x ^ (x >> 1); }
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
# define ctz64(x) __builtin_ctzll(x)
#else
static inline uint64_t ctz64(uint64_t x) {
   unsigned n = 1;
   if ((x & 0xffffffff) == 0) n += 32, x >>= 32;
   if ((x & 0x0000ffff) == 0) n += 16, x >>= 16;
   if ((x & 0x000000ff) == 0) n +=  8, x >>=  8;
   if ((x & 0x0000000f) == 0) n +=  4, x >>=  4;
   if ((x & 0x00000003) == 0) n +=  2, x >>=  2;
   return n - (x & 1);
}
#endif
static inline uint64_t gray_change(uint64_t x) {
    return ctz64(gray_code(x) ^ gray_code(x + 1));
}

#define D 0

static value find_cut_partition(struct vertex **g, unsigned n,
				unsigned *s, unsigned n_s,
				unsigned *t,
				bool *is_s, bool *is_t, unsigned k) {
    if(D) { dump_graph(g, n); putchar('\n'); }

#if D
    fprintf(stderr, "s = ");
    for (unsigned i = 0; i < n_s; i++)
	fprintf(stderr, "%d ", s[i]);
    fprintf(stderr, "\nt = ");
    for (unsigned i = 0; i < n_s; i++)
	fprintf(stderr, "%d ", t[i]);
    fprintf(stderr, "\n");
#endif
    
    for (unsigned i = 0; i < k; i++) {	
	if (augment_many_many(g, n, s, n_s, is_t) == (unsigned) -1) {
	    assert(i == k - 1);
	    goto found_cut;
	}
	if (D) { fprintf(stderr, "prepass %d\n", i); dump_graph(g, n); putchar('\n'); }
    }

    uint64_t code = 0;
    while (code < (((uint64_t) 1) << (n_s - 1))) {
	unsigned a = gray_change(code);
	unsigned augmentations = 0;

	while (vertex_flow(g, t[a])) {
	    ++augmentations;
	    unsigned target = drain_target(g, n, t[a], is_s);
	    if (D) { fprintf(stderr, "unaugmented %d -> s, target = %d\n", t[a], target); dump_graph(g, n); putchar('\n'); }
	    assert(target != (unsigned) -1);	    
	}
	while (vertex_flow(g, s[a])) {
	    ++augmentations;
	    unsigned target = drain_source(g, n, s[a], is_t);
	    if (D) { fprintf(stderr, "unaugmented t -> %d, target = %d\n", s[a], target); dump_graph(g, n); putchar('\n'); }
	    assert(target != (unsigned) -1);
	}

	is_s[s[a]] = false;
	is_t[t[a]] = false;

	unsigned tmp = t[a];
	t[a] = s[a];
	s[a] = tmp;

	is_s[s[a]] = true;
	is_t[t[a]] = true;

#if D
	fprintf(stderr, "s = ");
	for (unsigned i = 0; i < n_s; i++)
	    fprintf(stderr, "%d ", s[i]);
	fprintf(stderr, "\nt = ");
	for (unsigned i = 0; i < n_s; i++)
	    fprintf(stderr, "%d ", t[i]);
	fprintf(stderr, "\n");
#endif
	for (unsigned i = 0; i < augmentations; i++) {
	    unsigned target = augment_many_many(g, n, s, n_s, is_t);
	    if (target == (unsigned) -1) {
		assert(i == augmentations - 1);
		if (D) { fprintf(stderr, "cannot augmented s -> t\n"); dump_graph(g, n); putchar('\n'); }
		goto found_cut;
	    }
	    if (D) { fprintf(stderr, "augmented s -> t, target = %d\n", target); dump_graph(g, n); putchar('\n'); }
	}

	++code;
    }

    return Val_int(0);		/* [] */

found_cut:;
    if (D) fprintf(stderr, "found cut\n");
    
    unsigned q[n];
    memcpy(q, s, n_s * sizeof *s);
    unsigned *qhead = q, *qtail = q + n_s;
    bool seen[n];
    memset(seen, 0, sizeof seen);
    for (unsigned i = 0; i < n_s; i++)
	seen[s[i]] = true;
    while (qhead != qtail) {
	unsigned v = *qhead++;
	//fprintf(stderr, "popped %d\n", v);
	for (unsigned i = 0; i < g[v]->deg; i++) {
	    unsigned w = g[v]->neighbors[i].neighbor;
	    if (!seen[w] && g[v]->neighbors[i].residual > 0) {
		seen[w] = true;
		*qtail++ = w;
		//fprintf(stderr, "pushed %d\n", w);
	    }
	}
    }

    value tail = Val_int(0);		/* [] */

    for (unsigned v = 0; v < n; v++) {
	for (unsigned i = 0; i < g[v]->deg; i++) {
	    unsigned w = g[v]->neighbors[i].neighbor;
	    //printf("%d %d %d %d\n", v, w, seen[v], seen[w]);
	    if (v < w && seen[v] != seen[w]) {
		if (D) fprintf(stderr, "cutedge %d %d\n", v, w);
		value p = caml_alloc_tuple(2);
		Store_field(p, 0, Val_int(v));
		Store_field(p, 1, Val_int(w));
		value head = caml_alloc_tuple(2);		
		Store_field(head, 0, p);
		Store_field(head, 1, tail);
		tail = head;
	    }
	}
    }
    return tail;
}

CAMLprim value c_find_cut_partition(value vg, value vs, value vt, value vk) {
    assert(Is_long(vk));
    unsigned k = Long_val(vk);
    assert(Is_block(vg));
    unsigned n = Wosize_val(vg);
    struct vertex *g[n];
    for (unsigned v = 0; v < n; v++) {
	value vertex = Field(vg, v);
	assert(Is_block(vertex));
	unsigned deg = Wosize_val(vertex);
	g[v] = alloca(sizeof (struct vertex) + deg * sizeof (struct neighbor));
	g[v]->deg = deg;
	for (unsigned j = 0; j < g[v]->deg; j++) {
	    value vneigh = Field(vertex, j);
	    assert(Is_block(vneigh));
	    assert(Wosize_val(vneigh) == 2);
	    assert(Is_long(Field(vneigh, 0)));
	    assert(Is_long(Field(vneigh, 1)));
	    unsigned neighbor = Long_val(Field(vneigh, 0));
	    unsigned residual = Long_val(Field(vneigh, 1));
	    g[v]->neighbors[j].neighbor = neighbor;
	    g[v]->neighbors[j].residual = residual;
	    g[v]->neighbors[j].flow = 0;
	}
    }

    assert(Is_block(vs));
    unsigned n_s = Wosize_val(vs);
    unsigned s[n_s];
    bool is_s[n], is_t[n];
    memset(is_s, 0, sizeof is_s);
    memset(is_t, 0, sizeof is_t);
    for (unsigned i = 0; i < n_s; i++) {
	assert(Is_long(Field(vs, i)));
	s[i] = Long_val(Field(vs, i));
	is_s[s[i]] = true;
	
    }

    assert(Is_block(vt));
    assert(Wosize_val(vt) == n_s);
    unsigned t[n_s];
    for (unsigned i = 0; i < n_s; i++) {
	assert(Is_long(Field(vt, i)));
	t[i] = Long_val(Field(vt, i));
	is_t[t[i]] = true;
    }

    //dump_graph(g, n); putchar('\n');
    return find_cut_partition(g, n, s, n_s, t, is_s, is_t, k);
}

#if 0
CAMLprim value c_flow_test(value array) {
    assert(Is_block(array));
    unsigned n = Wosize_val(array);
    struct vertex *g[n];
    for (unsigned v = 0; v < n; v++) {
	value vertex = Field(array, v);
	assert(Is_block(vertex));
	unsigned deg = Wosize_val(vertex);
	g[v] = alloca(sizeof (struct vertex) + deg * sizeof (struct neighbor));
	g[v]->deg = deg;
	for (unsigned j = 0; j < g[v]->deg; j++) {
	    value vneigh = Field(vertex, j);
	    assert(Is_block(vneigh));
	    assert(Wosize_val(vneigh) == 2);
	    assert(Is_long(Field(vneigh, 0)));
	    assert(Is_long(Field(vneigh, 1)));
	    unsigned neighbor = Long_val(Field(vneigh, 0));
	    unsigned residual = Long_val(Field(vneigh, 1));
	    g[v]->neighbors[j].neighbor = neighbor;
	    g[v]->neighbors[j].residual = residual;
	}
    }
    dump_graph(g, n); putchar('\n');

    unsigned s[] = { 0 };
    bool is_t[n];
    memset(is_t, 0, sizeof is_t);

    is_t[6] = true;
    augment_many_many(g, n, s, 1, is_t);
    dump_graph(g, n); putchar('\n');

    augment_one_many(g, n, s, 1, is_t);
    dump_graph(g, n); putchar('\n');

    augment_one_many(g, n, s, 1, is_t);
    dump_graph(g, n); putchar('\n');

    printf("----------------------------\n");
    s[0] = 6;
    is_t[6] = false;
    is_t[0] = true;

    augment_one_many(g, n, s, 1, is_t);
    dump_graph(g, n); putchar('\n');

    augment_one_many(g, n, s, 1, is_t);
    dump_graph(g, n); putchar('\n');

    augment_one_many(g, n, s, 1, is_t);
    dump_graph(g, n); putchar('\n');

    return Val_unit;
}
#endif