unit UnitSettingLimitTimeFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.DateTimeCtrls,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Layouts,
  Lb.SysUtils;

type
  TSettingLimitTimeFrame = class(TFrame)
    TimeEditBegin: TTimeEdit;
    TimeEditEnd: TTimeEdit;
    Text1: TText;
    Rectangle: TRectangle;
    CheckBoxIsLimit: TCheckBox;
    LayoutTille: TLayout;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  end;

implementation

{$R *.fmx}

{ TSettingLevelFrame }

constructor TSettingLimitTimeFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSettingLimitTimeFrame.Destroy;
begin

  inherited;
end;

procedure TSettingLimitTimeFrame.Load;
begin
  CheckBoxIsLimit.IsChecked := ParamPlatform.IsLimitTime;
  TimeEditBegin.Time := ParamPlatform.TimeBegin;
  TimeEditEnd.Time := ParamPlatform.TimeEnd;
end;

procedure TSettingLimitTimeFrame.Save;
begin
  ParamPlatform.IsLimitTime := CheckBoxIsLimit.IsChecked;
  ParamPlatform.TimeBegin := TimeEditBegin.Time;
  ParamPlatform.TimeEnd   := TimeEditEnd.Time;
end;

end.
