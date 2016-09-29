unit UOptionsRecord;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 


type
  OptionsRecord = class

Private

Public
  textColour : Boolean ;
  Constructor init ;

end;


implementation

Constructor OptionsRecord.init;
begin
  self.textColour := True;
end;


end.

