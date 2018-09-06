unit formFriendsInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, ButtonPanel, uFriend;

type

  { TfrmFriendsInput }

  TfrmFriendsInput = class(TForm)
    btnPnlFriendsInput: TButtonPanel;
    chckBxAddtoEvents : TCheckBox;
    dtEdtDOB          : TDateEdit;
    edtHomePage       : TEdit;
    edtCounty         : TEdit;
    edtCountry        : TEdit;
    edtPostCode       : TEdit;
    edtCity           : TEdit;
    edtAddress2       : TEdit;
    edtAddress1       : TEdit;
    edtHouseNumber    : TEdit;
    edtTelNo1         : TEdit;
    edtTelNo2         : TEdit;
    edtTelNo3         : TEdit;
    edtFirstName      : TEdit;
    edtEmail1         : TEdit;
    edtLastName       : TEdit;
    edtEmail3         : TEdit;
    edtMiddleName     : TEdit;
    edtEmail2         : TEdit;
    Label1            : TLabel;
    Label10           : TLabel;
    Label11           : TLabel;
    Label12           : TLabel;
    Label13           : TLabel;
    Label14           : TLabel;
    Label15           : TLabel;
    Label16           : TLabel;
    Label2            : TLabel;
    Label3            : TLabel;
    Label4            : TLabel;
    Label5            : TLabel;
    Label6            : TLabel;
    Label7            : TLabel;
    Label8            : TLabel;
    Label9            : TLabel;
    moNotes           : TMemo;
    Panel1            : TPanel;

    procedure CancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public

  end;

var
  frmFriendsInput: TfrmFriendsInput;
  ReallyCanClose : Boolean;


implementation

uses
  formklock;

{$R *.lfm}

{ TfrmFriendsInput }

procedure TfrmFriendsInput.FormCreate(Sender: TObject);
begin
  kLog.writeLog('formFriendsInput Create');
end;

procedure TfrmFriendsInput.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

  if not ReallyCanClose then
    CloseAction := caNone
  else
    kLog.writeLog('formFriendsInput Close');
end;
//
// ********************************************************* Pannel Buttons *********
//
procedure TfrmFriendsInput.CancelButtonClick(Sender: TObject);
{  The cancel button has been pressed, so we forget any changes.
   We don't need to do nowt.
}
begin
  kLog.writeLog('formFriendsInput Close Clicked ');
  ReallyCanClose := true;
  Close
end;

procedure TfrmFriendsInput.HelpButtonClick(Sender: TObject);
begin
  kLog.writeLog('formFriendsInput Help Clicked ');
  ShowMessage('No Help, yet');
end;

procedure TfrmFriendsInput.OKButtonClick(Sender: TObject);
VAR
  f: friend;
begin
  kLog.writeLog('formFriendsInput OK Clicked ');

  if (edtFirstName.Text = '') or (edtLastName.Text = '') then
  begin
    showmessage('A valid First name and last name must be entered');
    ReallyCanClose := false;
    exit;
  end;

  ReallyCanClose := true;

  kLog.writeLog(format('[TfrmFriendsInput.OKButtonClick] Adding friend %d', [fr.friendsCount]));
  f := Friend.Create(fr.friendsCount);

  f.fName    := edtFirstName.Text;
  f.mName    := edtMiddleName.Text;
  f.sName    := edtLastName.Text;
  f.email1   := edtEmail1.Text;
  f.email2   := edtEmail2.Text;
  f.email3   := edtEmail3.Text;
  f.telNo1   := edtTelNo1.Text;
  f.telNo2   := edtTelNo2.Text;
  f.telNo3   := edtTelNo3.Text;
  f.houseNo  := edtHouseNumber.Text;
  f.address1 := edtAddress1.Text;
  f.address2 := edtAddress2.Text;
  f.city     := edtCity.Text;
  f.postCode := edtPostCode.Text;
  f.county   := edtCounty.Text;
  f.country  := edtCountry.Text;
  f.webPage  := edtHomePage.Text;
  f.dob      := DateTimeToStr(dtEdtDOB.Date);
  f.notes    := moNotes.Text;

  kLog.writeLog(format('[TfrmFriendsInput.OKButtonClick] %s %s %s', [f.sName, f.mName, f.fName]));

  fr.New(f);

  f.Free;
end;

end.

