void swap4c(char i[], int n) {
  int j;
  int m;
  char hold;

  for (m=0; m<n; m+=4){
    hold =i[m+0];
    i[m+0]=i[m+3];
    i[m+3]=hold;

    hold =i[m+1];
    i[m+1]=i[m+2];
    i[m+2]=hold;
    
  }
}
