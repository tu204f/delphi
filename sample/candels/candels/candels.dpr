program candels;

{$R 'db.res' 'lib\res\db.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Candel.Source in 'lib\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in 'lib\Lb.Candel.SysUtils.pas',
  Lb.ChartsFrame in 'lib\Lb.ChartsFrame.pas' {ChartsFrame: TFrame},
  Lb.Candel.Vector in 'lib\Lb.Candel.Vector.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Resource.Script in '..\..\..\library\Lb.Resource.Script.pas',
  Lb.Candel.DB in 'lib\Lb.Candel.DB.pas',
  Lb.SysUtils.ISO860 in '..\..\..\library\Lb.SysUtils.ISO860.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
