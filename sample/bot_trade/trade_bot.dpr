program trade_bot;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Candel.Source in 'src\Lb.Candel.Source.pas',
  Lb.SysUtils.Candel in 'src\Lb.SysUtils.Candel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
