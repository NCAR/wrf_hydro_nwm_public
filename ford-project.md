src_dir: trunk/NDHMS/HYDRO_drv
         trunk/NDHMS/Routing
         trunk/NDHMS/Data_Rec/
         trunk/NDHMS/LandModel/		
         trunk/NDHMS/LandModel_cpl/		
         trunk/NDHMS/MPP/		
         trunk/NDHMS/Routing/		
         trunk/NDHMS/nudging/
exclude_dir: trunk/NDHMS/LandModel/HRLDAS_forcing/run/examples
output_dir: doc
project_github: https://github.com/NCAR/wrf_hydro_nwm
project_website: https://ral.ucar.edu/projects/wrf_hydro
summary: The National Water Model Instance of WRF-Hydro
author: David Gochis et al.
fpp_extensions: F
fixed_extensions: GG
predocmark: >
media_dir: ./media
docmark_alt: #
predocmark_alt: <
display: public
         protected
         private
source: false
graph: true
search: true
preprocess: true
include: ./ford-includes
macro:	HYDRO_REALTIME=1
	        WRFIO_NCD_LARGE_FILE_SUPPORT=1
			WRF_HYDRO=1
			WRF_HYDRO_NUDGING=1
			SPATIAL_SOIL=1
			MPP_LAND=1
			PRECIP_DOUBLE=0
			HYDRO_D=0
			OUTPUT_CHAN_CONN=0
			NCEP_WCOSS=0

Hi, my name is Dave.

This is a project which I wrote. This file will provide the documents. I'm
writing the body of the text here. It contains an overall description of the
project. It might explain how to go about installing/compiling it. It might
provide a change-log for the code. [[linalg]] Maybe it will talk about the
history and/or motivation for this software.

@Note
You can include any notes (or bugs, warnings, or todos) like so.

@Bug
You can have multi-paragraph versions of these too! That means you can
include

- ordered lists
- unordered lists
- images
- etc.

Isn't that cool?
@endbug

@Bug Hey I'm doing it again...

This ones ends mid...@endbug ...paragraph.

You can have as many paragraphs as you like here and can use headlines, links,
images, etc. Basically, you can use anything in Markdown and Markdown-Extra.
Furthermore, you can insert LaTeX into your documentation. So, for example,
you can provide inline math using like \( y = x^2 \) or math on its own line
like \[ x = \sqrt{y} \] or $$ e = mc^2. $$ You can even use LaTeX environments!
So you can get numbered equations like this:
\begin{equation}
  PV = nRT
\end{equation}
So let your imagination run wild. As you can tell, I'm more or less just
filling in space now. This will be the last sentence.
