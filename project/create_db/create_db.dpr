program create_db;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Create.DB in 'lib\Lb.Create.DB.pas',
  Lb.FieldFrame in 'lib\frame\field\Lb.FieldFrame.pas' {FieldFrame: TFrame},
  Lb.IndexFrame in 'lib\frame\index\Lb.IndexFrame.pas' {IndexFrame: TFrame},
  Lb.ModulesFrame in 'lib\frame\module\Lb.ModulesFrame.pas' {ModuleTableFrame: TFrame},
  Lb.SysUtils in 'lib\Lb.SysUtils.pas',
  Lb.ModuleFrame in 'lib\frame\module\Lb.ModuleFrame.pas' {ModuleFrame: TFrame},
  Lb.FieldTableFrame in 'lib\frame\field\Lb.FieldTableFrame.pas' {FieldTableFrame: TFrame},
  Lb.IndexTableFrame in 'lib\frame\index\Lb.IndexTableFrame.pas' {IndexTableFrame: TFrame},
  Lb.MethodFrame in 'lib\frame\method\Lb.MethodFrame.pas' {MethodFrame: TFrame},
  Lb.MethodTableFrame in 'lib\frame\method\Lb.MethodTableFrame.pas' {MethodTableFrame: TFrame},
  UnitLayoutForm in 'UnitLayoutForm.pas' {LayoutForm},
  Lb.DataModuleDB in '..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.TreeViewFrame in 'lib\frame\tree_view\Lb.TreeViewFrame.pas' {TreeViewFrame: TFrame},
  Lb.ApplicationVersion in '..\..\library\Lb.ApplicationVersion.pas',
  Lb.CommandLine in '..\..\library\Lb.CommandLine.pas',
  Lb.Core.Events in '..\..\library\Lb.Core.Events.pas',
  Lb.Core.Variants in '..\..\library\Lb.Core.Variants.pas',
  Lb.Logger in '..\..\library\Lb.Logger.pas',
  Lb.SysUtils.ISO860 in '..\..\library\Lb.SysUtils.ISO860.pas',
  Lb.SysUtils.Structure in 'lib\Lb.SysUtils.Structure.pas',
  Lb.Resource in 'lib\res\Lb.Resource.pas' {ResFrame: TFrame},
  Lb.DomainFrame in 'lib\frame\domain\Lb.DomainFrame.pas' {DomainFrame: TFrame},
  Lb.DomainTableFrame in 'lib\frame\domain\Lb.DomainTableFrame.pas' {DomainTableFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
