unit UOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, EditBtn;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    ClrBtnLabel: TColorButton;
    GroupBox1: TGroupBox;
    lblLabelColour: TLabel;
    Panel1: TPanel;
    procedure btnExitClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ClrBtnLabelColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 


  {  create a options objects, that when created, will be used by main
     form. This will enable to change certain options i.e colout and font.

     TODO :: save to file i.e. xml                                          }
  OptionsRecord = class

  Private

  Public
    textColour : TColor ;             //  Global colour of all main labels
    Constructor init ;
    procedure settextColour(c : TColor);  //  used to set textColour
  end;


var
  frmOptions: TfrmOptions;
  OptionsRec : OptionsRecord;              //  Options record
implementation

{$R *.lfm}

{ TfrmOptions }

{  ** objects methods  **  }
Constructor OptionsRecord.init;
begin
  self.textColour := clBlack;
end;

procedure OptionsRecord.settextColour(c : TColor);
{  used to set textColour [global colour of all main labels]   }
begin
  self.textColour := c;
end;

{  ** form procedures  **  }
procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  OptionsRec := OptionsRecord.Create;  //  create options record,
                                       //  can then be used in main form

  ClrBtnLabel.ButtonColor   := OptionsRec.textColour ;  // in case different
end;

procedure TfrmOptions.OKButtonClick(Sender: TObject);
{ if ok clicked, change options record  }
begin
  OptionsRec.settextColour(ClrBtnLabel.ButtonColor);
end;

procedure TfrmOptions.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOptions.CancelButtonClick(Sender: TObject);
{  if cancel clicked, revert to previous options record.  }
begin
  lblLabelColour.Font.Color := OptionsRec.textColour ;
  ClrBtnLabel.ButtonColor   := OptionsRec.textColour ;
end;


procedure TfrmOptions.ClrBtnLabelColorChanged(Sender: TObject);
begin
  lblLabelColour.Font.Color := ClrBtnLabel.ButtonColor ;
end;


procedure TfrmOptions.Panel1Click(Sender: TObject);
begin

end;

end.

