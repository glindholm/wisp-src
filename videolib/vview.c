    1                           /************************************************************************/
    2                           /*                    UniView forms editor and converter                */
    3                           /*                      Copyright (c) 1988, 1989, 1990                  */
    4                           /*       An unpublished work of International Digital Scientific Inc.   */
    5                           /*                          All rights reserved.                        */
    6                           /************************************************************************/
    7       
    8       
    9       /*                                  Include standard header files.                                                  
        */
   10       
   11       #include <ctype.h>                                                                          /* Get character type macros
.       */
   12       #include <stdio.h>                                                                          /* Reference standard I/O.
        */
   13       #include <v/video.h>
   14       #include <v/vintdef.h>
   15       #include <v/vplus.h>
   16       #include <v/vform.h>
   17       #include <v/vlocal.h>
   18       #include <v/vdata.h>
   19       
   20       static first_form = TRUE;
   21       static int show_window();
   22       static int dag();
   23       
   24       /*                                          Entry point.                                                            
        */
   25       
   26       vview(infile, read_only, debugging) char *infile; int read_only, debugging;
   27       {
   28           int i,j,last,next;
   29           char data[1920];                                                                        /* Working data area.   
        */
   30           unsigned char *save, *vsss();
   31           struct video_form *form;
   32           char fd[32];
   33           int fn, ini, fie, fin, dtp;
   34           char *pp, *vgetproc();
   35           int displaying;
   36       
   37           vstate(DEFAULT);                                                                        /* Set default state.   
        */
   38           verase(FULL_SCREEN);                                                                    /* Erase the full screen.
        */
   39           vscreen(NARROW | DARK);                                                                 /* Select known screen size.
        */
   40       
   41           if (!vloadfd(infile))
   42           {
   43                   vbell();
   44                   vre_window("view: file %s not found.",infile);                                  /* Report file not found.
        */
   45                   verase(FULL_SCREEN);
   46                   vexit();
   47                   exit();
   48           }
   49       
   50           displaying = TRUE;
   51           form = vformdata;                                                                       /* Calculate where the data 
is.     */
   52           j = 1;
   53       
   54           while (displaying)
   55           {
   56                   form->keys_on = TRUE;                                                           /* Turn the function keys on
.       */
   57                   form->clear_screen = TRUE;                                                      /* Clear the screen.    
        */
   58                   form->start_row = 0;                                                            /* Assume the start row is 0
.       */
   59                   if (form->window_line == 0) form->start_row = 1;                                /* Move past the window line
.       */
   60                   form->control = 0;                                                              /* Make it read/write.  
        */



   61                   for (i = 0; i < 1920; i++) data[i] = ' ';                                       /* Initialize to spaces.
        */
   62                   vkeyset("DISPLAY NEXT    DISPLAY PREVIOUSDISPLAY NEW FORMAPPEND  NEW FORMFORM    INFO    FIELD   INFO    PRI
NT   FORM    EXIT            ");
   63                   i = vform(form,data,window_message);                                            /* Display the form.    
        */
   64                   switch (i)
   65                   {
   66                           case 1:
   67                           {
   68                                   j++;
   69                                   if (j > form_count)
   70                                   {
   71                                           j--;
   72                                           vre_window("Form %d is already the last form.",j);
   73                                   }
   74                                   else form++;
   75                                   break;
   76                           }
   77                           case 2:
   78                           {
   79                                   j--;
   80                                   if (j < 1)
   81                                   {
   82                                           j++;
   83                                           vre_window("Form %d is the first form.",j);
   84                                   }
   85                                   else form--;
   86                                   break;
   87                           }
   88                           case  8:
   89                           {
   90                                   displaying = FALSE;
   91                                   break;
   92                           }
   93                   }
   94           }   /* end while loop */
   95       
   96           verase(FULL_SCREEN);
   97           vmove(23,0); vmode(0);
   98           vdefer(RESTORE);
   99           vexit(); 
		exit();
