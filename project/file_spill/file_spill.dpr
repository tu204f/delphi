program file_spill;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Setting in '..\..\library\setting\Lb.Setting.pas',
  Lb.ConfigIniFile in '..\..\library\setting\Lb.ConfigIniFile.pas',
  Lb.Logger in '..\..\library\Lb.Logger.pas',
  Lb.SysUtils in 'lib\Lb.SysUtils.pas',
  Lb.ApplicationVersion in '..\..\library\Lb.ApplicationVersion.pas',
  Lb.Params in 'lib\Lb.Params.pas',
  Lb.SearchFile in 'lib\Lb.SearchFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
