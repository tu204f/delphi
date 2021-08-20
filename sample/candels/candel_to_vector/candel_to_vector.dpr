program candel_to_vector;

{$R 'db.res' '..\candels\lib\res\db.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitSourceDataFrame in 'lib\UnitSourceDataFrame.pas' {SourceDataFrame: TFrame},
  Lb.Candel.Blocks in '..\candels\lib\Lb.Candel.Blocks.pas',
  Lb.Candel.Source in '..\candels\lib\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in '..\candels\lib\Lb.Candel.SysUtils.pas',
  Lb.Candel.DB in '..\candels\lib\Lb.Candel.DB.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Resource.Script in '..\..\..\library\Lb.Resource.Script.pas',
  Lb.SysUtils.ISO860 in '..\..\..\library\Lb.SysUtils.ISO860.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  UnitParamVectorFrame in 'lib\UnitParamVectorFrame.pas' {ParamVectorFrame: TFrame},
  Lb.ChartsFrame in '..\candels\lib\source\Lb.ChartsFrame.pas' {ChartsFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
