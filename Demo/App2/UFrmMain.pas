unit UFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DzTalkApp, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    TA: TDzTalkApp;
    M: TMemo;
    procedure TAMessage(Sender: TObject; From: HWND; ID: Word; P: Pointer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.StrUtils;

type
  TRecordData = packed record
    Number: Integer;
    Text: ShortString;
    Flag: Boolean;
  end;

procedure TForm1.TAMessage(Sender: TObject; From: HWND; ID: Word; P: Pointer);
var R: TRecordData;
begin
  if ID<=1000 then
  begin
    M.Lines.Add('Command received: '+IntToStr(ID));
  end else
  case ID of
    1001: M.Lines.Add('Integer received: '+IntToStr(TA.AsInteger));
    1002: M.Lines.Add('String received: '+WideString(TA.AsString));
    1003: M.Lines.Add('Double received: '+FloatToStr(Double(P^)));
    1004:
      begin
        R := TRecordData(P^);

        M.Lines.Add(Format('Record received: Number=%d / Text=%s / Flag=%s',
          [R.Number, R.Text, IfThen(R.Flag, 'TRUE', 'FALSE')]));
      end;

    else M.Lines.Add(Format('Unknown command ID: %d', [ID]));
  end;
end;

end.
