program App2;

uses
  Vcl.Forms,
  UFrmMain in 'UFrmMain.pas' {Form1},
  UFrmStream in 'UFrmStream.pas' {FrmStream};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFrmStream, FrmStream);
  Application.Run;
end.
