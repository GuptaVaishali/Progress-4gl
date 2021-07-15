/*------------------------------------------------------------------------
   File        : EmailServerI.i 
   Purpose     : Interface for EmailServerDA.cls 
   Syntax      : 
   Description :         
   Author(s)   : Ruchi Gupta
   Created     : Thurs Jun 16 17:24:31 2021
   Notes       :     
 ----------------------------------------------------------------------*/  

INTERFACE EmailServerI:   
	 
	{pdsEmailServer.i   &reference-only="REFERENCE-ONLY"}  

	METHOD PUBLIC VOID GetEmailServer(
		INPUT-OUTPUT DATASET pdsEmailServer).   
	
	METHOD PUBLIC CHARACTER ReadDBTableEntry( 
		INPUT ipcServerPort AS CHARACTER).

END INTERFACE.
		