        >> SOURCE FORMAT IS FREE
identification division.
program-id. worldcities0.
*> 
*>  Copyright (C) 2014 Steve Williams <stevewilliams38@gmail.com>
*> 
*>  This program is free software; you can redistribute it and/or
*>  modify it under the terms of the GNU General Public License as
*>  published by the Free Software Foundation; either version 2,
*>  or (at your option) any later version.
*>  
*>  This program is distributed in the hope that it will be useful,
*>  but WITHOUT ANY WARRANTY; without even the implied warranty of
*>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*>  GNU General Public License for more details.
*>  
*>  You should have received a copy of the GNU General Public
*>  License along with this software; see the file COPYING.
*>  If not, write to the Free Software Foundation, Inc.,
*>  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

*> =============================================
*> 1) read a CSV file and create some run counts
*> =============================================

*> =======================================================
*>  I made a mistake in the first version of this program.

*>  The geoname.org documentation for the city-file
*>  showed geonameid, population, elevation and dem
*>  as binary fields.

*>  So I coded them as binary fields in the city-columns
*>  definitions in the COBOL programs.

*>  And, to my shame, I wrote some uncomplimentary words
*>  about ignorant programmers putting binary fields in
*>  CSV files.

*>  In this version I tried to extract the presumed
*>  binary fields before the unstring command, but I
*>  couldn't find them.

*>  So I wrote the HEX dump you'll see in the debug
*>  section of this program.

*>  The dump showed the presumed binary fields were
*>  actually unsigned display decimal fields.

*>  My apologies to the maligned programmer.
*> =======================================================

*> =======================================================
*> For those who wonder why I wrote the HEX dump as a
*> lookup into a 256 entry table, rather than extracting
*> the left and right nibbles:

*> You can mark some characters of interest in the table.
*> For example, change the x'09' (tab) entry in the first
*> row to '><' and see what the dump looks like.
*> =======================================================

environment division.
configuration section.
repository. function all intrinsic.
input-output section.
file-control.
    select city-file
        assign to city-file-name
        file status is city-file-status
        organization is line sequential.

data division.
file section.
fd  city-file.
copy "copy/city-record.cpy".

working-storage section.
01  city-file-name     pic x(64) value spaces.
01  city-file-status   pic x(2).
    88 success  value "00".
    88 eof      value "10".

01  input-count  pic 9(7) value zero.
01  city-count   pic 9(7) value zero.

01  city-columns.
    03  geonameid        pic 9(9).
    03  name             pic x(200).
    03  asciiname        pic x(200).
    03  alternatenames   pic x(5000).
    03  latitude         pic s9(3)v9(6).
    03  longitude        pic s9(3)v9(6).
    03  featureclass     pic x.
    03  featurecode      pic x(10).
    03  countrycode      pic x(2).
    03  cc2              pic x(60).
    03  admin1code       pic x(60).
    03  admin2code       pic x(80).
    03  admin3code       pic x(20).
    03  admin4code       pic x(20).
    03  population       pic 9(9).
    03  elevation        pic 9(5).
    03  dem              pic 9(5).
    03  timezone         pic x(40).
    03  modificationdate pic x(10).

01  city-lengths.
    03  name-length            pic 9(3).
    03  asciiname-length       pic 9(3).
    03  alternatenames-length  pic 9(4).
    03  cc2-length             pic 9(2).
    03  admin1code-length      pic 9(2).
    03  admin2code-length      pic 9(2).
    03  admin3code-length      pic 9(2).
    03  admin4code-length      pic 9(2).
    03  timezone-length        pic 9(2).

01  current-time.
    03  ct-hour       pic 99.
    03  ct-minute     pic 99.
    03  ct-second     pic 99.
    03  ct-hundredth  pic 99.

01  start-seconds           pic 9(7)v99.
01  end-seconds             pic 9(7)v99.
01  elapsed-seconds         pic 9(5)v99.
01  display-elapsed-seconds pic zz,zz9.99.
01  display-count           pic z,zzz,zz9.

01  cdx             pic 9999.
01  bdx             pic 999.
01  byte-count      pic 9.
01  bytes-per-word  pic 9 value 4.
01  word-count      pic 9.
01  words-per-line  pic 9 value 8.

01  hex.

*>  show tabs as '><'
*>    03  filler pic x(32) value '000102030405060708><0A0B0C0D0E0F'.

    03  filler pic x(32) value '000102030405060708090A0B0C0D0E0F'.
    03  filler pic x(32) value '101112131415161718191A1B1C1D1E1F'.
    03  filler pic x(32) value '202122232425262728292A2B2C2D2E2F'.
    03  filler pic x(32) value '303132333435363738393A3B3C3D3E3F'.
    03  filler pic x(32) value '404142434445464748494A4B4C4D4E4F'.
    03  filler pic x(32) value '505152535455565758595A5B5C5D5E5F'.
    03  filler pic x(32) value '606162636465666768696A6B6C6D6E6F'.
    03  filler pic x(32) value '707172737475767778797A7B7C7D7E7F'.
    03  filler pic x(32) value '808182838485868788898A8B8C8D8E8F'.
    03  filler pic x(32) value '909192939495969798999A9B9C9D9E9F'.
    03  filler pic x(32) value 'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'.
    03  filler pic x(32) value 'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'.
    03  filler pic x(32) value 'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'.
    03  filler pic x(32) value 'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'.
    03  filler pic x(32) value 'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'.
    03  filler pic x(32) value 'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'.

01  newline  pic x value x'0A'.
01  tab      pic x value x'09'.

procedure division chaining city-file-name.
declaratives.
    io-error section.
        use after error procedure on city-file.
    error-routine.
        if not success and not eof 
            display
                'File error with file status '
                with no advancing
            end-display
            display city-file-status end-display
            stop run
        end-if.
end declaratives.
start-worldcities0.
    display newline 'starting worldcities0' newline end-display

    display 'reading ' city-file-name newline end-display
    display 'selecting featureclass P : city, village,...' end-display

    open input city-file

    accept current-time from time end-accept
    compute start-seconds =
        ct-hour * 60 * 60
        + ct-minute * 60
        + ct-second
        + ct-hundredth / 100
    end-compute

    read city-file end-read

    perform until eof
        add 1 to input-count end-add

        initialize city-columns
        unstring city-record delimited by tab into
            geonameid
            name count in name-length
            asciiname count in asciiname-length
            alternatenames count in alternatenames-length
            latitude
            longitude
            featureclass
            featurecode
            countrycode
            cc2 count in cc2-length
            admin1code count in admin1code-length
            admin2code count in admin2code-length
            admin3code count in admin3code-length
            admin4code count in admin4code-length
            population
            elevation
            dem
            timezone count in timezone-length
            modificationdate
        end-unstring

*>         compile with -fdebugging-line to see some records and a HEX dump
>>D        if  input-count > 1000 and < 1003
>>D
>>D            display '--------------------------------' end-display
>>D            display 'RECORD OF geonameid=' geonameid end-display
>>D            display '--------------------------------' end-display
>>D            display 'name = ' name(1:name-length) end-display
>>D            display 'asciiname = ' asciiname(1:asciiname-length) end-display
>>D            display 'alternatenames = ' alternatenames(1:alternatenames-length) end-display
>>D            display 'latitude = ' latitude end-display
>>D            display 'longitude = ' longitude end-display
>>D            display 'featureclass = ' featureclass end-display
>>D            display 'featurecode = ' featurecode end-display
>>D            display 'countrycode = ' countrycode end-display
>>D            display 'cc2 = ' cc2(1:cc2-length) end-display
>>D            display 'admin1code = ' admin1code(1:admin1code-length) end-display
>>D            display 'admin2code = ' admin2code(1:admin2code-length) end-display
>>D            display 'admin3code = ' admin3code(1:admin3code-length) end-display
>>D            display 'admin4code = ' admin4code(1:admin4code-length) end-display
>>D            display 'population = ' population end-display
>>D            display 'elevation = ' elevation end-display
>>D            display 'dem = ' dem end-display
>>D            display 'timezone = ' timezone(1:timezone-length) end-display
>>D            display 'modificationdate = ' modificationdate end-display
>>D            display space end-display
>>D            display 'HEX DUMP:' end-display
>>D
>>D            move 1 to byte-count
>>D            move 1 to word-count
>>D            perform varying cdx from 1 by 1
>>D            until cdx > length(city-record)
>>D            or city-record(cdx:) = spaces
>>D                 compute bdx = 2 * ord(city-record(cdx:1)) - 1 end-compute
>>D                 display hex(bdx:2) with no advancing end-display
>>D                 add 1 to byte-count end-add
>>D                 if byte-count > bytes-per-word
>>D                     display ' ' with no advancing end-display
>>D                     move 1 to byte-count
>>D                     add 1 to word-count end-add
>>D                 end-if
>>D                 if word-count > words-per-line
>>D                     display ' ' end-display
>>D                     move 1 to word-count
>>D                 end-if
>>D            end-perform
>>D            if word-count <> 1
>>D            or byte-count <> 1
>>D                display ' ' end-display
>>D            end-if
>>D            display ' ' end-display
>>D        end-if

        if featureclass = 'P'
*>          ==========================================================
*>          what's this? see //www.geonames.org/export/codes.html
*>              A : country, state, region,...
*>              H : stream, lake, ...
*>              L : parks,area, ...
*>              P : city, village,...
*>              R : road, railroad 
*>              S : spot, building, farm
*>              T : mountain,hill,rock,... 
*>              U : undersea
*>              V : forest,heath,...
*>          ==========================================================
            add 1 to city-count end-add
        end-if

        read city-file end-read
        *> if not success
        *>    display city-file-status space with no advancing end-display
        *> end-if
    end-perform

    accept current-time from time end-accept
    compute end-seconds =
        ct-hour * 60 * 60
        + ct-minute * 60
        + ct-second
        + ct-hundredth / 100
    end-compute

    close city-file

    move city-count to display-count
    display display-count ' cities' end-display

    move input-count to display-count
    display display-count ' input records' end-display

    compute elapsed-seconds = end-seconds - start-seconds end-compute
    move elapsed-seconds to display-elapsed-seconds
    display display-elapsed-seconds ' elapsed seconds' end-display

    compute display-count =  input-count / elapsed-seconds
        on size error move 0 to display-count
    end-compute
    display display-count ' records per second' end-display

    display newline 'ending worldcities0' newline end-display

    stop run
    .
end program worldcities0.

