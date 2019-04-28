unit uweddingAnniversary;

{  A Wedding gift class.
   A wrapper around a list of wedding gifts for a given year.

   A call to whichGift(year), will return the apporopiate wedding gift for that year.
   The year is expected to be a integer.

   The list of the wedding gifts are held in a txt file in the application directory,
   this is read on initial create.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  WeddingGifts = class

  private
    _giftsFilename: string;

    property giftsFilename: string read _giftsFilename write _giftsFilename;

    procedure loadGifts;
  public
    constructor Create; overload;
    destructor Destroy; override;

    function whichGift(year: integer): string;
  end;

  //  effectively a dictionary.
  keyStore = specialize TFPGMap<integer, string>;

VAR
  giftStore: keyStore;

implementation

uses
  formklock;


constructor WeddingGifts.Create; overload;
{  set up some variables on create.    }
begin
  giftStore     := keyStore.Create;       //  initial giftStore
  giftsFilename := 'WeddingGifts.txt';
  loadGifts;
end;

destructor WeddingGifts.Destroy;
{  run on destroy.    }
begin
  giftStore.free;
  inherited;
end;

function WeddingGifts.whichGift(year: integer): string;
{  Returns the given gift for a given year.
   If no gift exist for that given year, return the string 'Choose your own'.
}
var
  gift: string;
begin
  if giftStore.TryGetData(year, gift) then
    result := gift
  else
    result := 'Choose your own';
end;

procedure WeddingGifts.loadGifts;
{  Populates Gifts with a list of wedding gifts.
   The gifts are be held in a text file in the application folder.
   The appropriate gift for a year should be at that index.

   Something screwy going on here.
   If I read the fist line, whch should be 1 - the strToInt gets "1" i.e. length 4 and not 1.
   This causes an exception with "1" is not a valid integer.
   Cured for the momonet with a dummy first line.
}
var
  line    : string;
  split   : TStringArray;
  giftFile: textFile;
  key     : integer;
  data    : string;
begin
  // Set the name of the file that will be read
  AssignFile(giftFile, giftsFilename);

  try
    // Open the file for reading
    reset(giftFile);

    // Keep reading lines until the end of the file is reached
    while not eof(giftFile) do
    begin
      readln(giftFile, line);
      split := line.Split(',');       //  split line into fields delimeted by ,

      if not split[0].Contains('#') then
      begin
        key  := strToInt(split[0]);
        data := split[1];

        giftStore.Add(key, data);
      end;  //  if split[0].Contains('#') then
    end;    //  while not eof(giftFile) do

    // Done so close the file
    CloseFile(giftFile);

  except
    on E: Exception do
    begin
      kLog.writeLog('ERROR: Reading WeddingGifts.txt :: ' + E.Message);
    end;  //  on E:
  end;    //  try
end;

end.

