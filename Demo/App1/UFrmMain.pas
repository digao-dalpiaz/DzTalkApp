unit UFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DzTalkApp, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    TA: TDzTalkApp;
    BtnSendCmd: TButton;
    EdID: TEdit;
    Label1: TLabel;
    BtnSendInteger: TButton;
    Label2: TLabel;
    EdInteger: TEdit;
    BtnSendString: TButton;
    Label3: TLabel;
    EdString: TEdit;
    BtnSendDouble: TButton;
    Label4: TLabel;
    EdDouble: TEdit;
    BtnSendRecord: TButton;
    Label5: TLabel;
    EdRecNumber: TEdit;
    Label6: TLabel;
    EdRecText: TEdit;
    CkRecFlag: TCheckBox;
    procedure BtnSendCmdClick(Sender: TObject);
    procedure BtnSendIntegerClick(Sender: TObject);
    procedure BtnSendStringClick(Sender: TObject);
    procedure BtnSendDoubleClick(Sender: TObject);
    procedure BtnSendRecordClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TRecordData = packed record
    Number: Integer;
    Text: ShortString;
    Flag: Boolean;
  end;

procedure TForm1.BtnSendCmdClick(Sender: TObject);
var ID: Word;
begin
  ID := StrToInt(EdID.Text);
  if ID>1000 then
    raise Exception.Create('ID bigger than 1000 not allowed on this demo app');

  TA.Send(ID);
end;

procedure TForm1.BtnSendIntegerClick(Sender: TObject);
begin
  TA.Send(1001, StrToInt( EdInteger.Text ) );
end;

procedure TForm1.BtnSendStringClick(Sender: TObject);
begin
  TA.Send(1002, AnsiString(EdString.Text));
end;

procedure TForm1.BtnSendDoubleClick(Sender: TObject);
var D: Double;
begin
  D := StrToFloat(EdDouble.Text);

  TA.Send(1003, @D, SizeOf(D));
end;

procedure TForm1.BtnSendRecordClick(Sender: TObject);
var R: TRecordData;
begin
  R.Number := StrToInt(EdRecNumber.Text);
  R.Text := ShortString(EdRecText.Text);
  R.Flag := CkRecFlag.Checked;

  TA.Send(1004, @R, SizeOf(R));
end;

end.
