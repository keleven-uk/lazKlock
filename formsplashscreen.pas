unit formSplashScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmSplashScreen }

  TfrmSplashScreen = class(TForm)
    Image1: TImage;
    MemoSplashScreenInfo: TMemo;
    MemoSplashScreenData: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

  private

  end;

var
  frmSplashScreen: TfrmSplashScreen;

implementation

{$R *.lfm}

{ TfrmSplashScreen }




procedure TfrmSplashScreen.FormClose(Sender: TObject;  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmSplashScreen := nil;
end;

end.

