#include <sys/resource.h>

static void hard( int resource, rlim_t lim )
{
    struct rlimit rl;
    rl.rlim_cur = lim;
    rl.rlim_max = lim;
    setrlimit( resource, &rl );
}

void masochist( void )
{
    hard( RLIMIT_AS, 100000000 );
    hard( RLIMIT_CORE, 0 );
    hard( RLIMIT_CPU, 10 );
    hard( RLIMIT_FSIZE, 1000000 );
    hard( RLIMIT_MEMLOCK, 100000000 );
    hard( RLIMIT_MSGQUEUE, 1000000 );
    hard( RLIMIT_NOFILE, 1000 );
    hard( RLIMIT_NPROC, 500 );
    hard( RLIMIT_STACK, 10000000 );
}

