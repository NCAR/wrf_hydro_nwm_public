/*
 get these projection functions from Seann Reed
 */
/*****************************************************

Name:  projection.h
 
Header file with projection constants.

*****************************************************/
#ifndef PROJECTION_H
#define PROJECTION_H

/*include files*/
#include <stdio.h>
#include <math.h>

/*define structures*/
struct point {
   double x;
	double y;
};

/*declare functions*/
//struct point albtogeo(struct point);
/*struct point geotohrap(struct point,float);*/
/*extern point albtogeo(point inpoint);*/
struct polyline {
   double box[4];
	int numparts;
	int numpoints;
	int *parts;
	struct point *points;
};

/*ellipsoid parameters*/
#define EQUATAXIS1866 6378206.4
#define EQUATAXISGRS80 6378137.0

#define E1866 0.0822719
#define E1980 0.0818192

#define E18662 0.006768658
#define E19802 0.0066943800

/*projection parameters*/
#define PI 3.1415927

/*Albers Equal Area*/
#define PARALLEL1 29.5
#define PARALLEL2 45.5
#define LATORIGIN 23.0
#define LONCENTER -96.0
#define N_ALB1866 0.6029035    /*Snyder 14-14*/
#define C_ALB1866 1.3491594    /*Snyder 14-13*/
#define PO_ALB1866 9929079.558  /*Snyder 14-12a*/
#define N_ALB1980 0.602902769    /*Snyder 14-14*/
#define C_ALB1980 1.349182032    /*Snyder 14-13*/
#define PO_ALB1980 9928937.004  /*Snyder 14-12a*/

/*HRAP Parameters*/
#define HRAPSTDLAT 1.0471976
#define HRAPLONCENTER 0.2617994
#define EARTHRAD 6371.2
#define SIZEAT60KM 4.7625
#define HRAPXOR 401
#define HRAPYOR 1601

#define SIZEAT60 4762.5


struct point geotohrap(struct point inpoint)
{
   double lon,lat;
   double psterx,pstery,R;
   double scalefact;
   double hrapx,hrapy;
   struct point outpoint;

   lon=inpoint.x;
   lat=inpoint.y;
   lon=lon*PI/180;
   lat=lat*PI/180;

   scalefact = (1+sin(HRAPSTDLAT))/(1+sin(lat));
   R = EARTHRAD*cos(lat)*scalefact;
   psterx = R*cos(lon+HRAPLONCENTER);
   pstery = R*sin(lon+HRAPLONCENTER);
   hrapx = (psterx/SIZEAT60KM+HRAPXOR);
   hrapy = (pstery/SIZEAT60KM+HRAPYOR);

   /*printf("%lf %lf %lf %lf\n",lon,lat,psterx,pstery);*/

   outpoint.x = hrapx;
   outpoint.y = hrapy;
  /*printf("hrapx hrapy %lf %lf\n",hrapx,hrapy);*/
   return (outpoint);
 }

struct point hraptogeo(struct point inpoint)
{
        double sterx,stery;
	double hrapx,hrapy;
	double lon,lat;
	double stdlon;
	double ang,bigr,arg;

        struct point outpoint;

        stdlon = -105.0;
        hrapx=inpoint.x;
        hrapy=inpoint.y;

        sterx = (hrapx - HRAPXOR)*SIZEAT60KM*1000;
        stery = (hrapy - HRAPYOR)*SIZEAT60KM*1000;

        bigr = pow((sterx*sterx + stery*stery),0.5);
        arg = bigr/(EARTHRAD*1000*(1+sin(HRAPSTDLAT)));
        lat = 90.0 - 2*(atan(arg)*180/PI);
        if ((sterx<0) & (stery<0))
           ang = (atan(stery/sterx)*180/PI*(-1)+180)*(-1);
        else if ((sterx<0) & (stery > 0))
           ang = atan(stery/sterx)*180/PI + 180;
        else
           ang = atan(stery/sterx)*180/PI;

        if (stery>0)
           ang = 270.0-stdlon-ang;
        else
           ang = -90-stdlon-ang;

        if (ang < 180.0)
           lon = -1*ang;
        else
           lon = 360.0 - ang;

        outpoint.x = lon;
        outpoint.y = lat;

        return (outpoint);
}

#endif //#ifndef PROJECTION_H
