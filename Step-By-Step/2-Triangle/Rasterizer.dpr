program Rasterizer;

uses
  Vcl.Forms,
  uForm in 'uForm.pas' {Form1},
  uObj in 'uObj.pas',
  uVectorGeometry in 'uVectorGeometry.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
