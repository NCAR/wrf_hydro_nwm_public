/*
    NASA/TRMM, Code 910.1.
    This is the TRMM Office Radar Software Library.
    Copyright (C) 1996, 1997
            John H. Merritt
            Space Applications Corporation
            Vienna, Virginia

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/
int big_endian(void);
int little_endian(void);
void swap_4_bytes(void *word);
void swap_2_bytes(void *word);

int big_endian(void)
{
  union {
    unsigned char byte[4];
    int val;
  } word;

  word.val = 0;
  word.byte[3] = 0x1;
  return word.val == 1;
}

int little_endian(void)
{
  union {
    unsigned char byte[4];
    int val;
  } word;
  word.val = 0;
  word.byte[3] = 0x1;
  return word.val != 1;
}


void swap_4_bytes(void *word)
{
  unsigned char *byte;
  unsigned char temp;
  byte    = word;
  temp    = byte[0];
  byte[0] = byte[3];
  byte[3] = temp;
  temp    = byte[1];
  byte[1] = byte[2];
  byte[2] = temp;
}

void swap_2_bytes(void *word)
{
  unsigned char *byte;
  unsigned char temp;
  byte    = word;
  temp    = byte[0];
  byte[0] = byte[1];
  byte[1] = temp;
}
