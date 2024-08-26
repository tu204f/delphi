unit UnitSettingBybitFrame;

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
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Objects,
  Lb.SysUtils;

type
  ///<summary>
  /// Настройки для байбит
  ///</summary>
  TSettingBybitFrame = class(TFrame)
    Text1: TText;
    Text3: TText;
    Text2: TText;
    EditSymble: TEdit;
    EditApiSecret: TEdit;
    EditApiKey: TEdit;
    CheckBoxVirtualTrade: TCheckBox;
    CheckBoxLogTrade: TCheckBox;
    CheckBoxIsTrend: TCheckBox;
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

{ TSettingBybitFrame }

constructor TSettingBybitFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSettingBybitFrame.Destroy;
begin

  inherited;
end;

procedure TSettingBybitFrame.Load;
begin
  EditSymble.Text    := ParamApplication.Symble;
  EditApiKey.Text    := ParamApplication.ApiKey;
  EditApiSecret.Text := ParamApplication.ApiSecret;
  CheckBoxLogTrade.IsChecked := ParamApplication.IsLogTrade;
  CheckBoxVirtualTrade.IsChecked := ParamApplication.IsVirtualChecked;
  CheckBoxIsTrend.IsChecked := ParamApplication.IsTrend;
end;

procedure TSettingBybitFrame.Save;
begin
  ParamApplication.Symble    := EditSymble.Text;
  ParamApplication.ApiKey    := EditApiKey.Text;
  ParamApplication.ApiSecret := EditApiSecret.Text;
  ParamApplication.IsVirtualChecked := CheckBoxVirtualTrade.IsChecked;
  ParamApplication.IsLogTrade := CheckBoxLogTrade.IsChecked;
  ParamApplication.IsVirtualChecked := CheckBoxVirtualTrade.IsChecked;
  ParamApplication.IsTrend := CheckBoxIsTrend.IsChecked;
end;

end.
