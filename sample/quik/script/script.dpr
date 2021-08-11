program script;

{$R 'script_qpl.res' '..\..\..\library\trade\script\script_qpl.rc'}

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {ScriptForm},
  Lb.Script.QPile in '..\..\..\library\trade\script\Lb.Script.QPile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TScriptForm, ScriptForm);
  Application.Run;
end.
