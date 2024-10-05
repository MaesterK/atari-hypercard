#include <stdio.h>
#include <string.h>

#define GAMEROMSIZE 1024*4
#define MENUEIMAGE ".\\roms\\menue.bin"
#define FINALIMAGE ".\\roms\\hypercard.bin"
#define MENUETEXTPOS (0xF300 & 0x0FFF)
#define MENUEIMAGEPOS 15
#define MENUETEXTLEN 12

int readromimage(char * filename,void* dest){
		FILE *gamerom;
	   gamerom = fopen(filename,"rb");

   if(gamerom == NULL)
   {
      printf("Fehler! Romimage %s nicht gefunden! \n",filename);   
      return -1;             
   }		 
   fread(dest, GAMEROMSIZE, 1, gamerom);
   
	fclose(gamerom);
	return 0;
}

int main(int argc, char** argv) {
	printf("Hello world!\n");
	int num;
	FILE *romlist;
	FILE *finalimage;
	char sbuff[255];
	char filename[200];
	char filepath[]=".\\roms\\";
	char filename_complete[255];
	int gametitlepos=0;
	char gametitle[255];
	
	char romimage[16][GAMEROMSIZE];//speicherbereich fuer image..15 spiele + menue je 4kB
	
   romlist = fopen("roms.lst","r");

   if(romlist == NULL)
   {
      printf("Error!");   
      return -1;             
   }

 //  printf("Enter num: ");
  // scanf("%d",&num);

 //  fprintf(fptr,"%d",num);
   
  //  while(fscanf(fptr,"%s",sbuff)!=EOF){ 
   	
   	
   	
   	   //menue an letzter stelle einbinden (galbaustein startet mit 1111 als auswahl)
   	readromimage(MENUEIMAGE,&romimage[MENUEIMAGEPOS][0]);	
   	
   	
 int gamepos=0;
    while(fgets(sbuff,sizeof(sbuff),romlist)!=NULL){
   		sscanf(sbuff,"%s",filename);//TODO maximale zeichenzahl begrenzen 	
    	gametitlepos=strlen(filename);

		//titel fuer menue extrahieren
		
   		//ggf carriage return entfernen		
		char* cr_nl=strchr(sbuff,'\r');
		if(cr_nl!=NULL){
			*cr_nl='\0';
		};
		cr_nl=strchr(sbuff,'\n');
		if(cr_nl!=NULL){
			*cr_nl='\0';
		};

   		strcpy(gametitle,&sbuff[gametitlepos+1]);
   		//in grossschreibung umwandeln		
   		strupr(gametitle);

   		
  		//stellen begrenzen 	
		if(strlen(gametitle)>MENUETEXTLEN){
			gametitle[MENUETEXTLEN]='\0';
			printf("titel zu lang! kuerze auf: %s\n",gametitle);
		}  
		  
		  
	//dateiladen und in generiertem image unterbringen
	strcpy(filename_complete,filepath);
	strcat(filename_complete,filename);
	if(readromimage(filename_complete,&romimage[gamepos][0])!=0){
		return -1;
	};	
         
      //titel in menue einfuegen      
      //gametitle[0]='A'+gamepos; //fuer test
      
      //textbereich mit leerzeichen initialisieren
      memset(&romimage[MENUEIMAGEPOS][MENUETEXTPOS+MENUETEXTLEN*gamepos],' ',12);
      //titel ohne nullterminierung in menue kopieren
      memcpy(&romimage[MENUEIMAGEPOS][MENUETEXTPOS+MENUETEXTLEN*gamepos],gametitle,strlen(gametitle));
      
		  	
   //		printf("%s\n",sbuff);
   	printf("dateiname: %s titel: %s hinzugefuegt\n",filename,gametitle);
   	gamepos++;
   	if(gamepos>16){
   		printf("zu viele roms in der liste...maximal 15!\n");
   		break;
	   }
   }
  fclose(romlist);  
  
     //image schreiben
      finalimage = fopen(FINALIMAGE,"wb");

   if(finalimage == NULL)
   {
      printf("Fehler! kann image nicht erstellen!");   
      return -1;             
   }

   fwrite(&romimage[0][0], GAMEROMSIZE, 16,finalimage);
 
    fclose(finalimage);  


	
	
	return 0;
}
