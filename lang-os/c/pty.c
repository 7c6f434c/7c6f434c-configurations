#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

int main(int argc, char ** argv, char ** envp)
{
	int fd, fd2, ptn, bit;
	char ptname[100];
	char readbuf[1025], writebuf[1025];
	size_t readc, writec;
	pid_t readpid,writepid,shellpid;
	int exitstatus;

	fd=open("/dev/ptmx", O_RDWR);

	ioctl(fd, TIOCGPTN, &ptn);
	
	bit=0;
	ioctl(fd, TIOCSPTLCK, &bit);

	snprintf(ptname, 99, "/dev/pts/%d", ptn);

	chmod(ptname, 0620);
	fd2=open(ptname, O_RDWR);

	if (!(readpid=fork())){
		while(1) {
			readc=read(0, readbuf, 1024);
			write(fd, readbuf, readc);
			if (! readc) close(fd);
		}
	}

	if (!(writepid=fork())){
		while(1) {
			writec=read(fd, writebuf, 1024);
			write(1, writebuf, writec);
		}
	}

	if (!(shellpid=fork())){
		dup2(fd2,0);
		dup2(fd2,1);
		dup2(fd2,2);

		setpgrp();
		setsid();

		execvp(argv[1], argv+1);
	}

	waitpid(shellpid, &exitstatus, 0);

	kill (readpid, SIGTERM);
	kill (writepid, SIGTERM);

	return exitstatus;
}
