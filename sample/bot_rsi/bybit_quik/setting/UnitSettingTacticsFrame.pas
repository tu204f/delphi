unit UnitSettingTacticsFrame;

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
  FMX.Controls.Presentation, FMX.Layouts, FMX.DateTimeCtrls;

type
  TSettingTacticsFrame = class(TFrame)
    GridLayout: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    CheckBoxIsTrend: TCheckBox;
    CheckBoxLogTrade: TCheckBox;
    CheckBoxVirtualTrade: TCheckBox;
    CheckBoxIsNewCandel: TCheckBox;
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

uses
  Lb.SysUtils;

{ TSettingTacticsFrame }

constructor TSettingTacticsFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSettingTacticsFrame.Destroy;
begin

  inherited;
end;

procedure TSettingTacticsFrame.Load;
begin
  CheckBoxLogTrade.IsChecked := ParamPlatform.IsLogTrade;
  CheckBoxVirtualTrade.IsChecked := ParamPlatform.IsVirtualChecked;
  CheckBoxIsTrend.IsChecked := ParamPlatform.IsTrend;
  CheckBoxIsNewCandel.IsChecked := ParamPlatform.IsNewCandel;
end;

procedure TSettingTacticsFrame.Save;
begin
  ParamPlatform.IsLogTrade := CheckBoxLogTrade.IsChecked;
  ParamPlatform.IsVirtualChecked := CheckBoxVirtualTrade.IsChecked;
  ParamPlatform.IsTrend := CheckBoxIsTrend.IsChecked;
  ParamPlatform.IsNewCandel := CheckBoxIsNewCandel.IsChecked;
end;

end.
