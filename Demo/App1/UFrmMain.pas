unit UFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DzTalkApp, Vcl.StdCtrls;

type
  TFrmMain = class(TForm)
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
    BtnSendStream: TButton;
    Label7: TLabel;
    LbResult: TLabel;
    procedure BtnSendCmdClick(Sender: TObject);
    procedure BtnSendIntegerClick(Sender: TObject);
    procedure BtnSendStringClick(Sender: TObject);
    procedure BtnSendDoubleClick(Sender: TObject);
    procedure BtnSendRecordClick(Sender: TObject);
    procedure BtnSendStreamClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

type
  TRecordData = packed record
    Number: Integer;
    Text: ShortString;
    Flag: Boolean;
  end;

procedure TFrmMain.BtnSendCmdClick(Sender: TObject);
var ID: Word;
begin
  ID := StrToInt(EdID.Text);
  if ID>1000 then
    raise Exception.Create('ID bigger than 1000 not allowed on this demo app');

  TA.Send(ID);
end;

procedure TFrmMain.BtnSendIntegerClick(Sender: TObject);
begin
  TA.Send(1001, StrToInt( EdInteger.Text ) );
end;

procedure TFrmMain.BtnSendStringClick(Sender: TObject);
begin
  TA.Send(1002, EdString.Text);
end;

procedure TFrmMain.BtnSendDoubleClick(Sender: TObject);
var D: Double;
begin
  D := StrToFloat(EdDouble.Text);

  TA.Send(1003, @D, SizeOf(D));
end;

procedure TFrmMain.BtnSendRecordClick(Sender: TObject);
var R: TRecordData;
begin
  R.Number := StrToInt(EdRecNumber.Text);
  R.Text := ShortString(EdRecText.Text);
  R.Flag := CkRecFlag.Checked;

  TA.Send(1004, @R, SizeOf(R));
end;

procedure TFrmMain.BtnSendStreamClick(Sender: TObject);

  procedure GetPrintScreen(var B: Vcl.Graphics.TBitmap);
  var DC: HDC;
    R: TRect;
  begin
    DC := GetDC(0);
    try
      Winapi.Windows.GetClientRect(WindowFromDC(DC), R);
      B.SetSize(R.Width, R.Height);

      BitBlt(B.Canvas.Handle, 0, 0, B.Width, B.Height, DC, 0, 0, SRCCOPY);
    finally
      ReleaseDC(0, DC);
    end;
  end;

var M: TMemoryStream;
  B: Vcl.Graphics.TBitmap;
begin
  M := TMemoryStream.Create;
  try
    B := TBitmap.Create;
    try
      GetPrintScreen(B);
      B.SaveToStream(M);
    finally
      B.Free;
    end;

    TA.Send(1005, M);
  finally
    M.Free;
  end;

  LbResult.Caption := IntToStr(TA.GetResult);
end;

end.
