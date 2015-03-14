IDL-Coyote is a library of IDL (Interactive Data Language) programs developed by David Fanning for teaching and illustrating IDL programming principles in IDL courses and on his IDL programming web page, [Coyote's Guide to IDL Programming](http://www.idlcoyote.com). Complete information about the [Coyote Graphics System (CGS)](http://www.idlcoyote.com/graphics_tips/coyote_graphics.php) can be found in David's latest
book, [Coyote's Guide to Traditional IDL Graphics](http://www.idlcoyote.com/books/index.php).


---

# News #

**12 January 2014**: Downloads of zip files have been disallowed from the Google Code SVN repositories. If you do not have an SVN client, and would prefer to download a zip file of the Coyote IDL libraries, you can find zip files on the program page of [Coyote's Guide to IDL Programming](http://www.idlcoyote.com/documents/programs.php) or in the [Coyote Store](http://www.idlcoyote.com/coyotestore/) under the Software tab. The Coyote Libraries are always free.

**22 May 2013**: There has been a major update of the Coyote Library today. I have added cgSet\_TTFont, which is used to select the True-Type font used to create graphical output. Because of the sticky nature of setting the True-Type fonts in the PostScript device, this required a reworking of most of the PostScript handling programs to keep the display and PostScript devices "in sync" with each other.

**19 February 2013**: Visit the [Coyote Plot Gallery](http://www.idlcoyote.com/gallery/index.php) to obtain IDL code and data to create well over 50 different types of IDL graphics plots.

**10 January 2013**: Added cgTaylorDiagram to the Coyote Library to make it easy to create Taylor diagrams from user data.

**5 January 2013**: Retired Coyote Library programs have been removed from the Coyote Library distribution and placed in their own branch of the trunk in the repository. They are still easy to access and will be available indefinitely, but as the Coyote Library continues to grow it seems unnecessary to carry them along with it.

**14 December 2012**: There has been a major change in the way color handling is done
in the Coyote Graphics Library today. You can learn about the details in the article
[Coyote Graphics Color Handling Changes](http://www.idlcoyote.com/cg_tips/colorupdate.php)
on the Coyote's Guide to IDL Programming web page.

**21 October 2012**: There has been a major clean-up and reorganization of the Coyote and Catalyst Libraries this weekend. A number of programs have been retired, some have had their names changed to better reflect their place in the Coyote Library, and some programs, including cgImage have been improved with new functionality. This would be an excellent time to update your Libraries if you haven't done so lately.

Note, too, that a new library, CoyotePlus, has been added to the repository. The programs in this library are mostly example programs that are used in IDL courses, but they are often instructive on their own and I wanted to make them more widely available.

The most recent version of the Coyote Library can be obtained from the Downloads page.

**27 July 2012**: The Coyote Library has undergone a major change today, with most of the graphics routines in the Library being updated to accept embedded symbols in axis and plot titles and other strings. This will make it much easier to use Greek characters and other plot symbols on graphics output. Any symbol recognized by cgSymbol can be used. The symbol must be have the characters "$\" attached to the front of the symbol name, and
the character "$" attached to the end of the symbol name. Here, for example, is a plot command that uses the Greek character mu and the Angstrom symbol: cgPlot, cgDemodata(1), XTitle='Length ($\mu$M)', YTitle='Distance ($\Angstrom$)'.

**20 July 2012**: The Coyote Library has undergone significant changes related to development of Lidar software to browse MABEL lidar data. In particular, upgrades have been made to SYMCAT (retired and replace with cgSymCat, which many new features), cgPlot, and cgZPlot. Please update your Library to the latest version.

**2 July 2012**: Create a new Coyote Library tagged release 2.0.0 to reflect the addition of the Coyote Map Routines into the basic Coyote Library.

**3 March 2012**: Created new Coyote Library download of tagged release 1.11.0, which includes
many updates and the addition of cgOPlot, a wrapper for cgPlot, which can serve as
a direct replacement for OPlot in IDL programs. Note that to keep up with current
changes (which can be _frequent_!), it is much easier to download the Coyote
Library directly from the [Coyote's Guide to IDL Programming web page](http://www.idlcoyote.com/programs/zip_files/coyoteprograms.zip).

**8 December 2011**: There have been significant changes to the Coyote Library this week. I think 28 of the 41 Coyote Graphics programs have been updated. Time to update your library!

Users can update to the latest tagged release by issuing an SVN command like this:

> svn co http://idl-coyote.googlecode.com/svn/tags/coyote_library_1.4 local\_dir

**20 August 2011**: Paul van Delst has convinced me to change the organization of this repository to support frequently updated tagged releases. To that end, I have created "coyote" and "catalyst" folders in the "tags" branch of the repository and I have renamed previously tagged releases to support the new naming convention for tagged releases. The "trunk" branch of the repository will still be used for current development work. As soon as I deem current work "stable," I will create a tagged release. I expect tagged releases to occur frequently.

Users can update to the latest tagged release by issuing an SVN command like this:

> svn co http://idl-coyote.googlecode.com/svn/tags/coyote_library_1.0.7 local\_dir

If you are interested in the very latest code development, or in older routines (say in the "retired" folder), you will find these in the "trunk" branch of the repository. For example:

> svn co http://idl-coyote.googlecode.com/svn/trunk/coyote/retired local\_dir

The tagged release naming convention will involve three numbers (e.g. 1.0.5). The first number will be a major release number. This number should change infrequently. The second number is a minor release number and will indicate new features in the library, such as programs with new keywords added, algorithm changes, etc. The third number is an update number and will indicate, primarily, bug fixes or documentation changes to current library programs.

**28 April 2011**: A bug-fix version of code originally released with the book,
_Coyote's Guide to Traditional IDL Graphics: Using Familiar Tools Creatively_,
has been created and tagged today. This corresponds to the state of the software
after returning from my "book tour" and fixing all known problems.
Users wanting the latest tagged release should use
a command like this:

svn co http://idl-coyote.googlecode.com/svn/tags/coyote_code_release_6_20110428 local\_dir

Personally, I think it is better to use the latest Coyote Library version from the
trunk of the repository. The trunk version is keep up to date and corresponds exactly
to the Coyote Library version available on my web page. The trunk version can be checked out with a command similar to this:

svn co http://idl-coyote.googlecode.com/svn/trunk/coyote local\_dir

**10 April 2011**: A bug-fix version of code originally released with the book,
_Coyote's Guide to Traditional IDL Graphics: Using Familiar Tools Creatively_,
has been created and tagged today. Users wanting the latest tagged release should use
a command like this:

svn co http://idl-coyote.googlecode.com/svn/tags/coyote_code_release_5_20110410 local\_dir

**7 March 2011**: The book, _Coyote's Guide to Traditional IDL Graphics: Using
Familiar Tools Creatively_, has been released to the printer. Books will be
shipped by the end of March. Meantime, the book can be purchased at a [pre-publication discount](http://www.idlcoyote.com/store) until 25 March 2011. To
correspond with the release, a new tagged release of the Coyote Library is made
available. These will be the Coyote Library routines described in the book. It
is _highly_ recommended you update to this tagged release, as there
have been a great many feature enhancements, bug fixes, and general testing of these
routines.

Users who want to get the released version should use a command such as this:

svn co http://idl-coyote.googlecode.com/svn/tags/coyote_code_release_4_20110307 local\_dir



**18 February 2011**: We created a new tagged release of the Coyote Library today which coincides with the the PDF release of my latest book, entitled _Coyote's Guide to Traditional IDL Graphics: Using Familiar Tools Creatively_.

Users who want to get the released version should use a command such as this:

svn co http://idl-coyote.googlecode.com/svn/tags/coyote_code_release_3_20110218 local\_dir

A zip file of this release is also available in the Downloads section.

The PDF version of the book is [available now](http://www.idlcoyote.com/store). The paperback edition of the book is expected sometime before the end of March 2011.

**3 January 2011**: We created a new tagged release of the Coyote Library today which implements Coyote Graphics routines discussed and used in the about-to-be-released new book, entitled _Coyote's Guide to Traditional IDL Graphics: Using Familiar Tools Creatively_. An announcement will be made when the book is ready to be published.

Users who want to get the released version should use a command such as this:

svn co http://idl-coyote.googlecode.com/svn/tags/coyote_code_release_2_20110103 local\_dir



**16 November 2010**: A number of new programs have been created, and old programs updated, to create device independent, color mode independent, graphics programs to replace the Plot, PlotS, Oplot, Contour, Surface, and Shade\_Surf commands in IDL. Have a look at FSC\_Plot, FSC\_PlotS, FSC\_Contour, FSC\_Surf, FSC\_Display, FSC\_Erase and all the rest.
Changes have been made to TVImage, TVScale, FSC\_Colorbar, and DCBar to name only a few
of the programs that have changed to accommodate the "new" old commands. This might be a good time to update your Coyote Library distribution. Additional information can be found on the [Coyote's Guide to IDL Programming](http://www.idlcoyote.com/graphics_tips/oldnewcmds.html) web page.

**12 October 2010**: A stable release of the Coyote and Catalyst Libraries was made today
in the "tags" section of the repository. Users who want to get the released version should
use a command such as this:

svn co http://idl-coyote.googlecode.com/svn/tags/coyote_code_release_1_20101012 local\_dir

The very latest changes to the Coyote and Catalyst libraries are still available from the "trunk" section of the repository.

svn co http://idl-coyote.googlecode.com/svn/trunk local\_dir

We expect to create released versions several times a year, as necessary.

**8 Oct 2010**: It has come to my attention that UNIX and Windows machines report the UTC time in the SysTime function differently. UNIX reports today's data as "Fri,  8 Oct 2010 21:53:26 GMT", whereas Windows reports it as "Fri, 08 Oct 2010 21:53:26 GMT". The problem lies in the way the day string is reported. I mean for the day string to always contain two digits (e.g., "08"), but this was not happening on UNIX machines. I've modified the TimeStamp code so that it always reports the day string as two digits on all computers.

**7 Oct 2010**: Adding "system" colors to FSC\_Color may have been the worst design decision I ever made. Since these require a window connection to determine, this decision has haunted me for years in the form of people wanting to use the software in places where there is no window connection. For example, in cron jobs. To accommodate legacy code, I need to **always** check for a connection. I am loath to do this because it means opening and closing a window, which is, relatively speaking, a slow process. I've made another attempt to solve this problem with this update and the corresponding update to the Coyote Library program CanConnect. CanConnect how creates the system variable !FSC\_Display\_Connection. If this system variable exists, FSC\_Color consults it to determine if there is a display connection. It it doesn't exist, it calls CanConnect. This way, a window has to be open only once in an IDL session to determine if a display connection can be made.

The Coyote Library routines FSC\_Color, CanConnect, and PickColorname have been updated, as well as the Catalyst Library program CatColors\_\_Define.

**5 Oct 2010**: Histoplot has been updated to allow multiple polygon fill colors with the POLYCOLOR keyword.

**4 Oct 2010**: Histoplot has been updated with better handling of NANs and "missing" data. Now all missing data is set to NANs before processing, which ensures that the reverse indices returned by the program will be correct.

**29 Sept 2010**: XColors has been updated to better allow user-supplied color table files to be used with the program. And a name mis-match with the Brewer color tables has been corrected in the fsc\_brewer.tbl file.

**27 Sept 2010**: IDL Coyote and Catalyst libraries have been updated to account for all known IDL 8 name space conflicts. At least 10 programs have changed. The major change is that COLORBAR has been moved to an "obsolete" directory and replaced by the FSC\_COLORBAR program in the Coyote Library. It is recommended that you update both your Coyote and Catalyst installations. Additional information about IDL 8 name conflicts can he found
in [this article](http://www.idlcoyote.com/ng_tips/idl8_name_conflicts.html).

**23 Sept 2010**: IDL Coyote and Catalyst libraries are now available via a Subversion repository.


---


# Downloads #

If you prefer not to download the library files from this Subversion repository,
you can download zip files containing the most recent files in the libraries from [Coyote's Guide to IDL Programming](http://www.idlcoyote.com/documents/programs.html):

  * [Coyote Library](http://www.idlcoyote.com/programs/zip_files/coyoteprograms.zip)
  * [Catalyst Library](http://www.idlcoyote.com/programs/zip_files/catalyst.zip)

[Installation instructions](http://www.idlcoyote.com/code_tips/installcoyote.php) are
available for installing both libraries for your particular IDL installation. Please
note that nearly all Catalyst Library modules depend upon Coyote Library programs, so if you want the functionality of the Catalyst Library, you will also need to install the Coyote Library.


---
