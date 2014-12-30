/* Note: g77 allows the name do_prompt to be translated as
 *   do_prompt, do_prompt_, or do_prompt__
 * by combining the options
 *   -fno-underscoring 
 *   -fno-second-underscore
 * There are also some case-preserving options.
 * If another compiler with readline is less flexible with regard to
 * the external names generated, by all means change the name 
 * do_prompt__ as needed and tell make what new options to use under g77
 */

#include <stdio.h>
/* THESE FILES FON'T SEEM TO EXIST, but are unneeded? :
#include <readline/readline.h>
#include <readline/history.h>
*/
#define maxprompt 80
void getinput_(char*promptstr, char*line, int promptlen, int linelen)
{
  char *line_read;
  char str[maxprompt+1];
  int i;     

  /* Convert the promptstr from fortran */
  for (i=0; i < maxprompt && i < promptlen; i++) str[i] = promptstr[i];
  str[i] = '\0';
  /* while (i && str[i-1]==' ') str[--i] = '\0'; */

  /* Get a line from the user. */
  line_read = readline (str);
     
  /* Save it to the history (even if it doesn't have any text in it) */
  if (line_read) add_history (line_read);

  /* Convert the line_read to fortran */
  if (line_read)
    for (i=0; line_read[i] && i < linelen; i++) line[i] = line_read[i];
  else
    i = 0;
  while (i < linelen) line[i++] = ' ';

  /* If the buffer has already been allocated, return the memory
     to the free pool. */
  if (line_read) free (line_read);
}
