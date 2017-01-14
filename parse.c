/*                                                                       */
/*                                                                       */
/*   FORMULA PARSER                                                      */
/*                                                                       */
/*      USER CALLABLE ROUTINES                                           */
/*                                                                       */
/*        (1) double evalform  (char **f, char *err)                     */
/*                                                                       */
/*            returns value of expression.                               */
/*                                                                       */
/*            *f      ASCII string containing formula.  Returns          */
/*                   pointer to end of string if formula is ok,          */
/*                   otherwise points to location of error.              */
/*            *err   Returns 0 if OK, >0 for error.  Use value to        */
/*                   call *parsemessage for error message                */
/*                                                                       */
/*        (2) char   *parsemsg (char err)                                */
/*                                                                       */
/*            returns pointer to error message                           */
/*                                                                       */
/*            err  error value returned by evalfrom                      */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*        FORMULA SYNTAX                                                 */
/*                                                                       */
/*          evalform can recognize the following items                   */
/*                                                                       */
/*          (1) number constants                                         */
/*          (2) constants "e" and "pi"                                   */
/*          (3) operators + - * / ^ ( ) =                                */
/*              (unlimited number of parenthesis)                        */
/*          (4) variables (up to MAXNUMBERVAR of them)                   */
/*          (5) single argument functions                                */
/*              ( SIN COS TAN EXP LOG LOG10 ACOS ASIN ATAN ABS SQRT )    */
/*                                                                       */
/*        VARIABLES                                                      */
/*                                                                       */
/*           () names can be any length - case is irrelevant             */
/*           () up to MAXNUMBERVAR variables                             */
/*           () set variables with =                                     */
/*                string "a = 5^2" sets variable A to 25.                */
/*           () variables used in right hand side of equation            */
/*              are taken to be zero if not initialized.                 */
/*           () can set multiple variables on one line.                  */
/*                string "a0 = a1 = a2 = sqrt(2)" sets all three         */
/*                variables to sqrt(2).                                  */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*
Revision History:

29 Apr 1996
   Added error codes for out-of-range function parameters

*/

/*
************************************************************************
Include Files
************************************************************************
*/
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "parse.h"

/*
************************************************************************
Defines
************************************************************************
*/
#define MAXNUMBERVAR  128
#define MAXTOKENLENGTH 32
#define FALSE 0
#define TRUE  1
#define NOTFOUND -1
#define NOROOM   -2
#define NOHEAP   -3
#define BOOLEAN int
#define DEFAULT_RETURN 0.0

#ifndef M_E
#define M_E        2.7182818284590452354E0
#endif
#ifndef M_PI
#define M_PI       3.1415926535897931160E0
#endif


/*
************************************************************************
Type Definitions
************************************************************************
*/

/*  OPERATORS (IN ORDER FROM LOWEST TO HIGHEST PRECEDENCE)   */
typedef enum
   {
   OP_EndLine,
   OP_BeginLine,
   OP_CloseParenthesis,
   OP_OpenParenthesis,
   OP_Assignment,
   OP_Add,
   OP_Subtract,
   OP_Multiply,
   OP_Divide,
   OP_RaisePower
   } operator_t;

typedef enum
   {
   FUNC_err,
   FUNC_sin,
   FUNC_cos,
   FUNC_tan,
   FUNC_exp,
   FUNC_log,
   FUNC_log10,
   FUNC_fabs,
   FUNC_acos,
   FUNC_asin,
   FUNC_atan,
   FUNC_sqrt,
   FUNC_int
   }   function_t;

typedef enum
   {
   ERROR_none,
   ERROR_operand,
   ERROR_openparen,
   ERROR_closeparen,
   ERROR_operator,
   ERROR_division,
   ERROR_function,
   ERROR_variable_expected,
   ERROR_variable_full,
   ERROR_variable_long,
   ERROR_heap_full,
   ERROR_parameter
   } errorcode_t;


/*
************************************************************************
MODULE-WIDE VARIABLES
************************************************************************
*/
static  char         *VariableNames_m   [MAXNUMBERVAR];
static  double        VariableValues_m [MAXNUMBERVAR];
static  char         *FormulaString_m;
static  char         TokenString_m[MAXTOKENLENGTH];
static  double       DivisorValue_m;
static  int          ParenthesisLevel_m;
static  errorcode_t   ErrorCode_m;
static  int          NumberOfVariables_m = 0;


/*
************************************************************************
Local Function Prototypes
************************************************************************
*/

/*  Prototype moved to parse.h  */
#if 0
int        AssignVariable ( char *NewName, double NewValue);
#endif

char        *_strhed (char **);
int        GetVariableID (char *TestName);
function_t LookupFunction (char *FunctionName);
double     EvaluateFunction (function_t InputFunction, double x);
static void       SkipWhiteSpace (char **f);
int        GetNextTokenLength ( char *cptr );
void        CopyUppercaseString
            ( char *TargetString, char *SourceString, int Size );
double     ParseFormula (operator_t *PendingOperator);


/*
************************************************************************
Local Subroutines
************************************************************************
*/



/*  RETURN Variable Index CORRESPONDING TO VARIABLE NAME  */
int GetVariableID (char *TestName)
   {
   int VariableID = NumberOfVariables_m-1;

   /*  SEARCH STORED VARIABLE NAMES  */
   for (VariableID=0; VariableID<NumberOfVariables_m; VariableID++)
      {
      if (strcmp( VariableNames_m [ VariableID ], TestName) == 0)
         /* RETURN CURRENT INDEX  */
         return VariableID;
      }
   /*  VARIABLE NAME NOT FOUND  */
   return NOTFOUND;
   }
/*
    ASSIGN VALUE TO STORED VARIABLE NAME or CREATE NEW VARIABLE
    RETURN     VARIABLE ID
           or ERROR CODE NOROOM IF NO LIST SPACE AVAILABLE
           or ERROR CODE NOHEAP IF NO HEAP SPACE AVAILABLE
*/
int AssignVariable ( char *NewName, double NewValue)
   {
   int VariableID;

   VariableID = GetVariableID (NewName);
   /*  CREATE NEW VARIABLE  */
   if (VariableID == NOTFOUND)
      {
      /*  IS THERE ROOM IN LIST FOR NEW VARIABLE  */
      if (NumberOfVariables_m >= MAXNUMBERVAR )
         return (NOROOM);
      /*  IS THERE ROOM IN HEAP FOR NEW VARIABLE NAME  */
      VariableNames_m [ NumberOfVariables_m ] =
         (char *) calloc ( strlen(NewName)+1, sizeof(char) );
      if ( VariableNames_m [ NumberOfVariables_m ] == NULL)
         return (NOHEAP);
      /*  STORE VARIABLE  */
      strcpy ( VariableNames_m [ NumberOfVariables_m ], NewName );
      VariableValues_m [ NumberOfVariables_m ] = NewValue;
      NumberOfVariables_m++;
      return ( NumberOfVariables_m-1 );
      }
   /*  STORE VALUE IN EXISTING VARIABLE  */
   else
      {
      VariableValues_m [ VariableID ] = NewValue;
      return VariableID;
      }
   }

function_t LookupFunction (char *FunctionName)
   {
   if (!strcmp(FunctionName, "SIN"  ))   return( FUNC_sin);
   if (!strcmp(FunctionName, "COS"  ))   return( FUNC_cos);
   if (!strcmp(FunctionName, "TAN"  ))   return( FUNC_tan);
   if (!strcmp(FunctionName, "EXP"  ))   return( FUNC_exp);
   if (!strcmp(FunctionName, "LOG"  ))   return( FUNC_log);
   if (!strcmp(FunctionName, "LOG10"))   return( FUNC_log10);
   if (!strcmp(FunctionName, "ACOS" ))   return( FUNC_acos);
   if (!strcmp(FunctionName, "ASIN" ))   return( FUNC_asin);
   if (!strcmp(FunctionName, "ATAN" ))   return( FUNC_atan);
   if (!strcmp(FunctionName, "ABS"  ))   return( FUNC_fabs);
   if (!strcmp(FunctionName, "SQRT" ))   return( FUNC_sqrt);
   if (!strcmp(FunctionName, "INT"  ))   return( FUNC_int );
   return(FUNC_err);
   }

double EvaluateFunction (function_t InputFunction, double x)
   {
   BOOLEAN Ok;

   /*  Check argument ranges   */
   Ok = TRUE;
   switch (InputFunction)
      {
      case FUNC_log:
      case FUNC_log10:
      case FUNC_sqrt:
         if (x<=0.0)
            {
            Ok = FALSE;
            }
         break;
      case FUNC_acos:
      case FUNC_asin:
         if (x<-1.0 || x>=1.0)
            {
            Ok = FALSE;
            }
      }

   /*  If arguments not ok, then return 0  */
   if (!Ok)
      {
      ErrorCode_m = ERROR_parameter;
      return DEFAULT_RETURN;
      }

   /*  Evaluate function  */
   switch (InputFunction)
      {
      case FUNC_sin    : return sin(x);
      case FUNC_cos    : return cos(x);
      case FUNC_tan    : return tan(x);
      case FUNC_exp    : return exp(x);
      case FUNC_log    : return log(x);
      case FUNC_log10 : return log10(x);
      case FUNC_fabs  : return fabs(x);
      case FUNC_acos  : return acos(x);
      case FUNC_asin  : return asin(x);
      case FUNC_atan  : return atan(x);
      case FUNC_sqrt  : return sqrt(x);
      case FUNC_int    :
         /* INSURES INT(-1.2) = -1, NOT -2 */
         if (x<0)
            return ceil(x);
         else
            return floor(x);
      default:
         printf("\nINTERNAL ERROR: unknown function sent to EvaluateFunction %i\n\n",
                  InputFunction);
            exit(1);
            /*  DUMMY RETURN TO PREVENT COMPILER WARNING  */
            return 0.0;
      }
   }


static void SkipWhiteSpace (char **f)
   {
   while (**f==' ' || **f==9 || **f==10 || **f==13)
      (*f)++;
   }


int GetNextTokenLength ( char *cptr )
   {
   int TokenLength;

   /* FIRST CHARACTER MUST BE LETTER OR PERCENT  */
   if (
        ! (  ( *cptr >= 'A' && *cptr <= 'Z' ) ||
             ( *cptr >= 'a' && *cptr <= 'z' ) ||
             ( *cptr == '%')
          )
      )
      return 0;

   /*  REMAINING CHARACTERS CAN BE LETTERS, NUMBERS OR UNDERSCORES  */
   TokenLength = 1;
   cptr++;
   while ( isalnum(*cptr) || *cptr=='_' )
      {
      cptr++;
      TokenLength++;
      }
   return TokenLength;
   }

void CopyUppercaseString ( char *TargetString, char *SourceString, int Size )
   {
   int ichar;

   for (ichar=0;ichar<Size;ichar++)
      TargetString[ichar] = toupper(SourceString[ichar]);
   /*  ADD STRING TERMINATOR   */
   TargetString[Size] = 0;
   }

double ParseFormula (operator_t *PendingOperator)
   {
   operator_t CurrentOperator;
   function_t CurrentFunction;
   double     CurrentValue;
   BOOLEAN     MinusSignPresent;
   BOOLEAN     ApplyOperator;
   int        VariableID;
   int        TokenLength;

                              /*  PARSE VALUE  */

   /*
      Values can take one of 5 forms
         - Parenthetical Expression
         - Numeric Constant
         - Function
         - Variable
         - Special Constant (ie e or pi )
   */

   /*  INITIALIZE VARIABLE ID TO NONE   */
   VariableID = NOTFOUND;

   /*  REMOVE OPENING WHITE SPACE   */
   SkipWhiteSpace (&FormulaString_m);

   /*  CHECK FOR MINUS SIGN   */
   MinusSignPresent = FALSE;
   if (*FormulaString_m=='-')
      {
      MinusSignPresent = TRUE;
      FormulaString_m++;
      }

   /*  .. OR PLUS SIGN   */
   else if (*FormulaString_m=='+')
      FormulaString_m++;


   /*  GET VALUE -- PARENTHETICAL EXPRESSION .. */
   if ( *FormulaString_m=='(' )
      {
      FormulaString_m++;
      ParenthesisLevel_m++;
      CurrentOperator = OP_OpenParenthesis;
      /*  OPERATOR SHOULD RETURN AS OP_CloseParenthesis ')'  */
      CurrentValue = ParseFormula (&CurrentOperator);
      if ( ErrorCode_m != ERROR_none )
         return DEFAULT_RETURN;
      if (CurrentOperator!=OP_CloseParenthesis)
         {
         ErrorCode_m = ERROR_openparen;
         return DEFAULT_RETURN;
         }
      }
   /* .. GET VALUE -- CONSTANT  */
   else if
   (
   (FormulaString_m[0] >= '0' && FormulaString_m[0] <= '9') ||
   FormulaString_m[0]=='.'
   )
      {

#ifdef TEST
      char *TempString;
      TempString = _strhed (&FormulaString_m);
      CurrentValue = atof (TempString);
#endif
      CurrentValue = strtod (FormulaString_m, &FormulaString_m);
      }
   /* .. GET VALUE -- NAME (EITHER FUNCTION, VARIABLE, SPECIAL CONSTANT)   */
   else
      {
      /*  SCAN NAME TOKEN IN FORMULA  */
      TokenLength = GetNextTokenLength ( FormulaString_m );
      /*  .. ERROR - NO NAME TOKEN FOUND   */
      if (TokenLength == 0)
      {
         ErrorCode_m = ERROR_operand;
         return DEFAULT_RETURN;
      }
      /*  .. ERROR - NAME TOKEN TOO LONG   */
      if (TokenLength >= MAXTOKENLENGTH )
         {
         ErrorCode_m = ERROR_variable_long;
         return DEFAULT_RETURN;
         }
      /*  REMOVE LEADING NAME TOKEN FROM FORMULA  */
      CopyUppercaseString (TokenString_m, FormulaString_m, TokenLength);
      FormulaString_m += TokenLength;
      /*  COMPARE TOKEN TO SPECIAL CONSTANTS  */
      if       (!strcmp(TokenString_m,"%E"))
         CurrentValue = M_E;
      else if   (!strcmp(TokenString_m,"%PI"))
         CurrentValue = M_PI;
      /*  INTERPRET FUNCTIONS  */
      else if ((CurrentFunction=LookupFunction (TokenString_m)) != FUNC_err )
         {
         /*  SKIP WHITE SPACE  */
         SkipWhiteSpace (&FormulaString_m);
         /*  GET VALUE -- PARENTHETICAL EXPRESSION .. */
         if (*FormulaString_m=='(')
            {
            FormulaString_m++;
            ParenthesisLevel_m++;
            CurrentOperator = OP_OpenParenthesis;
            /*  PARSE FUNCTION ARGUEMENT   */
            CurrentValue = ParseFormula (&CurrentOperator);
            /*  PASS ANY ERROR BACK UP TO CALLING ROUTINE  */
            if ( ErrorCode_m != ERROR_none )
               return DEFAULT_RETURN;
            /*  IF NO CLOSE PARENTHESIS - RETURN ERROR  */
            if (CurrentOperator!=OP_CloseParenthesis)
               {
               ErrorCode_m = ERROR_openparen;
               return DEFAULT_RETURN;
               }
            }
         /*  MISSING OPERAND AFTER FUNCTION NAME  */
         else
            {
            ErrorCode_m = ERROR_operand;
            return DEFAULT_RETURN;
            }

         /*  EVALUATE FUNCTION  */
         CurrentValue = EvaluateFunction (CurrentFunction, CurrentValue);

         /*  Test for error  */
         if (ErrorCode_m != ERROR_none)
            {
            return DEFAULT_RETURN;
            }
      }
      /*  GET VALUE ... VARIABLE  */
      else
         {
         VariableID = GetVariableID(TokenString_m);
         if (VariableID==NOTFOUND)
            {
            /*  CREATE VARIABLE - INITIALIZE TO ZERO   */
            VariableID = AssignVariable (TokenString_m, 0.0);
            if (VariableID == NOROOM)
               /*  VARIABLE LIST FILLED  */
               {
               ErrorCode_m = ERROR_variable_full;
               return DEFAULT_RETURN;
               }
            else if (VariableID == NOHEAP )
               /*  HEAP FILLED  */
               {
               ErrorCode_m = ERROR_heap_full;
               return DEFAULT_RETURN;
               }
            }
         CurrentValue = VariableValues_m[VariableID];
         }
      }

   /*  APPLY UNARY OPERATOR TO NUMBER */
   if (MinusSignPresent)
      CurrentValue = -CurrentValue;

                             /*   PARSE OPERATOR  */

   /* REMOVE LEADING WHITESPACE   */
   SkipWhiteSpace (&FormulaString_m);

   /*  GET OPERATOR   */
   switch (*FormulaString_m) {
      case '+' : CurrentOperator = OP_Add;                 break;
      case '-' : CurrentOperator = OP_Subtract;            break;
      case '*' : CurrentOperator = OP_Multiply;            break;
      case '/' : CurrentOperator = OP_Divide;              break;
      case '^' : CurrentOperator = OP_RaisePower;          break;
      case ')' : CurrentOperator = OP_CloseParenthesis;    break;
      case '=' : CurrentOperator = OP_Assignment;          break;
      case '\0': CurrentOperator = OP_EndLine;             break;
      default   : ErrorCode_m = ERROR_operator;
                 return DEFAULT_RETURN;
   }
   ++FormulaString_m;  /* increment pointer beyond operator  */


   /*  APPLY OPERATOR IF NO HIGHER PENDING OPERATORS   */
   ApplyOperator = TRUE;
   while (ApplyOperator)
      {
      /*  CRITERIA FOR APPLYING OPERATOR   */
      if ( ErrorCode_m == ERROR_none )
         {
         /*  EVALUATE REPEATING ASSIGNMENT OPERATORS RIGHT TO LEFT  */
         if (CurrentOperator==OP_Assignment)
            ApplyOperator = ( CurrentOperator >= *PendingOperator );
         /*  ... OTHERWISE EVALUATE FROM LEFT TO RIGHT  */
         else
            ApplyOperator = ( CurrentOperator >  *PendingOperator );
      }
      else
         ApplyOperator = FALSE;

      if (ApplyOperator)
         {
         switch (CurrentOperator)
            {
            case OP_Add:
               CurrentValue += ParseFormula (&CurrentOperator);
               break;
            case OP_Subtract:
               CurrentValue -= ParseFormula (&CurrentOperator);
               break;
            case OP_Multiply :
               CurrentValue *= ParseFormula (&CurrentOperator);
               break;
            case OP_Divide :
               DivisorValue_m = ParseFormula(&CurrentOperator);
               if ( DivisorValue_m == 0 )
                  {
                  ErrorCode_m = ERROR_division;
                  CurrentValue = 0.0;
                  }
               else
                  CurrentValue /= DivisorValue_m;
               break;
            case OP_RaisePower:
               CurrentValue  =
                  pow (CurrentValue, ParseFormula (&CurrentOperator));
               break;
            case OP_CloseParenthesis :
               ParenthesisLevel_m--;
               if ( ParenthesisLevel_m<0 )
                  ErrorCode_m = ERROR_closeparen;
               break;
            case OP_Assignment:
               if (VariableID == NOTFOUND)
                  {
                  /*  VARIABLE EXPECTED  */
                  ErrorCode_m = ERROR_variable_expected;
                  return DEFAULT_RETURN;
                  }
               VariableValues_m[ VariableID ]
                  = CurrentValue = ParseFormula ( &CurrentOperator);
               break;
            }
         }
      }

   *PendingOperator = CurrentOperator;
   return (CurrentValue);
   }


/*  PULLS AND RETURNS POINTER TO FIRST TOKEN  */
char *_strhed (char **tadd)
{
   char *head, *sep, *tail;
   tail = (*tadd);
   /* FIND FIRST NONBLANK CHARACTER  */
   while (    (*tail)
          && ( (*tail)==' ' || (*tail)=='\t' || (*tail)=='\n' )  )
             tail++;
   /* FIND NEXT WHITE SPACE CHARCTER  */
   head = (sep = tail);
   while ((*sep) && (*sep)!=' ' && (*sep)!='\n' && (*sep)!='\t') sep++;
   /* INSERT NULL CHARACTER  */
   tail = sep;
   if (*sep)
      { (*sep) = 0;
        tail++;
      }
   /*  RETURN FIRST TOKEN   */
   SkipWhiteSpace (&tail);
   *tadd = tail;
   return(head);
}


/*
************************************************************************
Exported Subroutines
************************************************************************
*/


char *listvar (int VariableID, double *val)
   {
   if (VariableID>=NumberOfVariables_m)
      return (NULL);
   else
      {
      *val = VariableValues_m[VariableID];
      return (VariableNames_m[VariableID]);
      }
   }

char *parsemsg (int InputErrorCode)
   {
   char *msg;

   switch ( (errorcode_t) InputErrorCode )
      {
      case ERROR_none:
         msg = "";
         break;
      case ERROR_operand:
         msg = "error: invalid operand.";
         break;
      case ERROR_openparen:
         msg = "error: unmatched left parenthesis.";
         break;
      case ERROR_closeparen:
         msg = "error: unmatched right parenthesis.";
         break;
      case ERROR_operator:
         msg = "error: invalid operator.";
         break;
      case ERROR_division:
         msg = "error: division by zero.";
         break;
      case ERROR_function:
         msg = "error: unknown function.";
         break;
      case ERROR_variable_expected:
         msg = "error: variable expected.";
         break;
      case ERROR_variable_full:
         msg = "error: variable space full.";
         break;
      case ERROR_variable_long:
         msg = "error: variable name too long.";
         break;
      case ERROR_heap_full:
         msg = "error: heap space full.";
         break;
      case ERROR_parameter:
         msg = "error: function parameter is out of range.";
         break;
      default:
         msg = "internal error:  Unknown error code.";
         break;
      }
   return(msg);
}

double evalform (char **f, int *ErrorResult)
   {
   double     ValueResult;
   operator_t CurrentOperator;

   /*  SET GLOBAL VARIABLES  */
   ErrorCode_m        = ERROR_none;
   ParenthesisLevel_m = 0;
   FormulaString_m    = *f;
   /*  SET INPUT OPERATOR   */
   CurrentOperator = OP_BeginLine;
   /*  CALL PARSEING ROUTINE   */
   ValueResult = ParseFormula (&CurrentOperator);
   /*  SET PARAMETER OUTPUT VARIALBES   */
   *ErrorResult  = (int ) ErrorCode_m;
   *f = FormulaString_m;
   return(ValueResult);
   }

/*  PULLS LEADING TOKEN: EVALUATES AS EXPRESSION AND RETURNS DOUBLE   */
double dblstrf (char **tadd)   {
   char *f;
   int  err;
   f = _strhed(tadd);
   return (evalform(&f, &err));
}

/*  PULLS LEADING TOKEN: EVALUATES AS EXPRESSION AND RETURNS INT      */
int    intstrf ( char **tadd) {
   return ( dblstrf (tadd) );
}

/*  PULLS LEADING TOKEN: EVALUATES AS EXPRESSION AND RETURNS LONG     */
long    lngstrf ( char **tadd) {
   return ( dblstrf (tadd) );
}
