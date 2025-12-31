union wait {
	int     w_status;               /* used in syscall */
	/*
	 * Terminated process status.
	 */
	struct {
		unsigned int    w_Termsig:7,    /* termination signal */
		    w_Coredump:1,               /* core dump indicator */
		    w_Retcode:8,                /* exit code if w_termsig==0 */
		    w_Filler:16;                /* upper bits filler */
	} w_T;
	/*
	 * Stopped process status.  Returned
	 * only for traced children unless requested
	 * with the WUNTRACED option bit.
	 */
	struct {
		unsigned int    w_Stopval:8,    /* == W_STOPPED if stopped */
		    w_Stopsig:8,                /* signal that stopped us */
		    w_Filler:16;                /* upper bits filler */
	} w_S;
};
