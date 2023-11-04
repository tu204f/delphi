program script;

{$R 'script_qpl.res' '..\..\..\library\trade\script\script_qpl.rc'}

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {ScriptForm},
  Lb.Script.QPile in '..\..\..\library\trade\script\Lb.Script.QPile.pas',
  Lb.Setting in '..\..\..\library\Lb.Setting.pas',
  QPILE.Script in '..\..\..\library\trade\script\QPILE.Script.pas',
  QPILE.AST in '..\..\..\library\trade\script\QPILE.AST.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TScriptForm, ScriptForm);
  Application.Run;
end.
