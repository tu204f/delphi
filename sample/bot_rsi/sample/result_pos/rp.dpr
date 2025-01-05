program rp;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.SysUtils in '..\..\bq_rsi_2\lb\Lb.SysUtils.pas',
  Lb.ParamPosition in '..\..\bq_rsi_2\lb\Lb.ParamPosition.pas',
  UnitParamPositionFrame in 'UnitParamPositionFrame.pas' {ParamPositionFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
