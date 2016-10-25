
/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
/\/\/\/\/\/\/ MEMORY  KIT /\/\/\/\/\/\/\
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
\/ An example Axiom by Kevin Horowitz \/
/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

To use this axiom, simply send "MEMKIT.8xv" to your calculator.
When compiling a program, just add #Axiom(MEMKIT) to the start
of the code (after the Axe header) and the new commands will be
available.  You can find the new Axiom tokens in the [Vars][2] menus.


This particular axiom is designed to give the programmer more control
over memory management of the calculator's external variables.


Special thanks to Runer112 who wrote the New() and Delete() commands.


Commands:
---------------------------------------------------------------------------------------------
Load()              Arguments: ---None---
                    Returns:   ---None---
                    Description: Gets the first program/appvar on the list.
---------------------------------------------------------------------------------------------             
Next()              Arguments: ---None---
                    Returns:   0 If no more programs/appvars to look at, non-zero otherwise.
                    Description: Gets the next program/appvar on the list.
---------------------------------------------------------------------------------------------             
Dim()               Arguments: ---None---
                    Returns: The current program/appvar's type.
                    Description: 5=Program, 6=Protected Program, 21=Appvar
---------------------------------------------------------------------------------------------            
Dim()r              Arguments: ---None---
                    Returns: The current program/appvar's data pointer.
                    Description: Its as if you had called GetCalc() on the program/appvar.
---------------------------------------------------------------------------------------------             
Dim()rr             Arguments: ---None---
                    Returns: The current program/appvar's page number.
                    Description: 0 means in ram, anything else is in archive.
---------------------------------------------------------------------------------------------           
Print(BUF)          Arguments: BUF = Location to copy name to.
                    Returns: ---None---
                    Description: Copies the appvar/program's name (plus ending 0) to a buffer.
---------------------------------------------------------------------------------------------            
New(PTR,OFS,SIZ)    Arguments: PTR = Pointer to start of program/appvar (from getcalc).
                               OFS = Offset in program to add memory
                               SIZ = Size of memory to insert
                    Returns: 0 if it failed, non-zero if it succeeded.
                    Description: Attempts to inflate the size of a program/appvar
---------------------------------------------------------------------------------------------
Delete(PTR,OFS,SIZ) Arguments: PTR = Pointer to start of program/appvar (from getcalc).
                               OFS = Offset in program to start deleting memory
                               SIZ = Size of memory to delete
                    Returns: 0 if it failed, non-zero if it succeeded.
                    Description: Attempts to shrink the size of a program/appvar
---------------------------------------------------------------------------------------------