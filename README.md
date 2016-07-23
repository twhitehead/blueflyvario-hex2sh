# blueflyvario-hex2sh

This program makes it possible to update the BlueFlyVario firmware
using only your Kobo.  It does this by converting the BlueSkyVario
PIC24F firmware hex files to sh script that you then copy onto your
Kobo and run to update your BlueSkyVario firmware.

I don't believe there is anything specific to the BlueFlyVario about
it though.  It should work with PIC24F that uses the
[ds30loader](http://www.ds30loader.com).

## Sample usage

Install the require haskell libraries (`Text.Trifecta` and
`Text.Parse`) and [download the zip
file](http://blueflyvario.com/support) corresponding to the firmware
you wish to flash onto your PIC24F device.  Unpack it and then run

```bash
runhaskell hex2sh.hs $FIRMWARE_FILE > update-firmware.sh
```

where `$FIRMWARE_FILE` is the name of the appropriate firmware hex
file you unpacked from the downloaded zip file (e.g.,
*BlueFlyVario_TTL_GPS_10.208.hex*).  Copy this shell script to your
XCSoar enabled Kobo.  Telnet into the Kobo and run the script

```bash
chmod +x update-firmware.sh
./update-firmware.sh
```

While the script is saying that it is "establishing communication with
ds30loader" power on (or power cycle) the BlueFlyVario with the PIC24F
RA0 pin high (programming pin PGC shorted to programming pin VDD).
See the [BlueFlyVario
blogs](http://blueflyvario.blogspot.ca/2014/12/firmware.html) for more
details on the pins and pictures.

## Notes

A reported verificaiton failure for address 0xf8000 can be safely
ignored.  This is the configuration range.  It reads back different
than written as some bits hardwired to zero.  It should likely be
filtered from the script as I'm also not sure if it is suppose to
written using the same procedure as the regular flash program memory
(i.e., the PIC24F manual doesn't seem to indicate it needs to be done
in row sized blocks or that it requires an erasure operation).
