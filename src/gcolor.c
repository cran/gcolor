#include <stdio.h>
#include <stdlib.h>


/* If you change MAX_NR_VERTICES, change MAX_NR_VERTICESdiv8 to be
the 1/8th of it */

#define MAX_NR_VERTICES		5000
#define MAX_NR_VERTICESdiv8	625
#define MAX_PREAMBLE 10000
#define MAX_FILE_NAME 200
#define BOOL	char


static char Preamble[MAX_PREAMBLE];
static char *inputFile = "fileToConvert.txt";

char fileToConvert[MAX_FILE_NAME];
char masks[ 8 ] = { 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80 };
BOOL Bitmap[MAX_NR_VERTICES][MAX_NR_VERTICESdiv8];
int Nr_vert, Nr_edges;

int get_params()
							 /* getting Nr_vert and Nr_edge from the preamble string,
						 containing Dimacs format "p ??? num num" */
{
	char c, *tmp;
	char * pp = Preamble;
	int stop = 0;
	tmp = (char *)calloc(100, sizeof(char));

	Nr_vert = Nr_edges = 0;

	while (!stop && (c = *pp++) != '\0'){
		switch (c)
		  {
			case 'c':
			  while ((c = *pp++) != '\n' && c != '\0');
			  break;

			case 'p':
			  sscanf(pp, "%s %d %d\n", tmp, &Nr_vert, &Nr_edges);
			  stop = 1;
			  break;

			default:
			  break;
		  }
	}

	free(tmp);

	if (Nr_vert == 0 || Nr_edges == 0)
	  return 0;  /* error */
	else
	  return 1;

}

void read_graph_DIMACS_bin()
{

	int i, length = 0;
	FILE *fp;

	printf("Reading File: %s",fileToConvert);
	fp=fopen(fileToConvert,"r");
	
	if ( fp==NULL )
	  { printf("ERROR: Cannot open infile %s\n", fileToConvert); }

	if (!fscanf(fp, "%d\n", &length))
	  { printf("ERROR: Corrupted preamble.\n"); }

	if(length >= MAX_PREAMBLE)
	  { printf("ERROR: Too long preamble.\n"); }

	fread(Preamble, 1, length, fp);
	Preamble[length] = '\0';

	if (!get_params())
		  { printf("ERROR: Corrupted preamble.\n"); exit(10); }

	for ( i = 0; i < Nr_vert && fread(Bitmap[i], 1, (int)((i + 8)/8), fp); i++ );

	fclose(fp);
}

BOOL get_edge(i, j)
	  int i;
	  int j;
{
	int byte, bit;
	char mask;

	bit  = 7-(j & 0x00000007);
	byte = j >> 3;

	mask = masks[bit];
	return( (Bitmap[i][byte] & mask)==mask );
}

void write_graph_DIMACS_ascii(file)
	  char *file;
{
	int i,j;
	FILE *fp;

	if( remove( file ) != 0 )
		printf( "Error deleting temporary file" );
	else
		printf( "Temporary file successfully deleted" );


	if ( (fp=fopen(file,"w"))==NULL )
	  { printf("ERROR: Cannot open outfile\n"); }

	fprintf(fp, Preamble);

	for ( i = 0; i<Nr_vert; i++ )
	  {
		  for ( j=0; j<=i; j++ )
			if ( get_edge(i,j) ) fprintf(fp,"e %d %d\n",i+1,j+1 );
	  }

	fclose(fp);
}

void read_file_to_convert() {
	FILE *fp;
	char fileName[MAX_FILE_NAME];
	int i;

	printf("Getting the name of the File To Convert.\n");
	
	fp=fopen(inputFile,"r");
	
	if ( fp==NULL )
	  { printf("ERROR: Cannot open fileToConvert.txt\n"); }

	fgets(fileName, MAX_FILE_NAME, fp);

	for (i = 0; i<MAX_FILE_NAME; i++ ) {
	    if(fileName[i]!='\n') {
		fileToConvert[i]=fileName[i];
	    }
	}

	printf("File To Convert is: -%s-\n", fileToConvert);

	fclose(fp);
}

void convertDIMACSFromBin2Ascii() {
	read_file_to_convert();
	read_graph_DIMACS_bin();
	write_graph_DIMACS_ascii("output.asc");
}
