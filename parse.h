#ifndef __PARSE_H
#define __PARSE_H

double evalform (char **f, int *err);
char *parsemsg (int err);
char *listvar  (int varid, double *val);
double  dblstrf (char **);
int     intstrf (char **);
long	  lngstrf (char **);
int     AssignVariable (char *, double);

#endif
