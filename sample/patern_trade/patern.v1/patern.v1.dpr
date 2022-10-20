program patern.v1;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Candel.SysUtils in '..\src\Lb.Candel.SysUtils.pas',
  Lb.Patern in '..\src\Lb.Patern.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
