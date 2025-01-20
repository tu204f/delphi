program project_1;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form4},
  Lb.Neoro.SysUtils in '..\lib\Lb.Neoro.SysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
