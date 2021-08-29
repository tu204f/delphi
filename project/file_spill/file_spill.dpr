program file_spill;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Setting in '..\..\library\setting\Lb.Setting.pas',
  Lb.ConfigIniFile in '..\..\library\setting\Lb.ConfigIniFile.pas',
  Lb.Logger in '..\..\library\Lb.Logger.pas',
  Lb.SysUtils in 'lib\Lb.SysUtils.pas',
  Lb.Threading in '..\..\library\Lb.Threading.pas',
  Lb.Params in '..\..\library\Lb.Params.pas',
  Lb.ApplicationVersion in '..\..\library\Lb.ApplicationVersion.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
