#define A 1
( A )
#define B(x) {x}
B(bla)

B (bla)

#ifdef A
OK
#else
BOOM
#endif

#ifdef unknown /* comment */
maybe
#endif

#if defined(A)
OK
#endif

#define __GLIBC_USE(X) __GLIBC_USE_ ## X

#if __GLIBC_USE (IEC_60559_BFP_EXT) || __GLIBC_USE (ISOC2X)
OK
#endif

#define answer 42

#if answer < 10
  A
#else
  B
#endif

#if defined __USE_GNU || defined __STDC_WANT_IEC_60559_FUNCS_EXT__
  A
#endif

#define X A

#define Y(A) B(A)
