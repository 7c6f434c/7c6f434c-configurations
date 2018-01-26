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
	size_t readc, forwardc, writec;
	pid_t readpid,writepid,shellpid;
	int exitstatus;

	fd=open("/dev/ptmx", O_RDWR);

	ioctl(fd, TIOCGPTN, &ptn);
	
	bit=0;
	ioctl(fd, TIOCSPTLCK, &bit);

	snprintf(ptname, 99, "/dev/pts/%d", ptn);

	chmod(ptname, 0620);
	fd2=open(ptname, O_RDWR);

	if (!(shellpid=fork())){
		dup2(fd2,0);
		dup2(fd2,1);
		dup2(fd2,2);

		setpgrp();
		setsid();

		execvp(argv[1], argv+1);
	}

	if (!(readpid=fork())){
                readc = -1;
                forwardc = -1;
		while(readc && forwardc) {
			readc=read(0, readbuf, 1024);
                        forwardc = write(fd, readbuf, readc);
		}
                close(0);
                close(fd);
                kill(shellpid,SIGHUP);
                while(1){}
	}

	if (!(writepid=fork())){
                writec = -1;
                forwardc = -1;
		while(writec && forwardc) {
			writec=read(fd, writebuf, 1024);
			forwardc = write(1, writebuf, writec);
		}
                close(fd);
                close(1);
                kill(shellpid,SIGPIPE);
                while(1){}
	}

	waitpid(shellpid, &exitstatus, 0);

	kill (readpid, SIGTERM);
	kill (writepid, SIGTERM);
	kill (readpid, SIGKILL);
	kill (writepid, SIGKILL);
	kill (readpid, SIGCONT);
	kill (writepid, SIGCONT);

	return WEXITSTATUS(exitstatus);
}
