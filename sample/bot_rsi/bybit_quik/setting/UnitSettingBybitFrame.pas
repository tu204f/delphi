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
  FMX.Layouts,
  UnitSettingTacticsFrame;

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
    LayoutTactics: TLayout;
  private
    SettingTactics: TSettingTacticsFrame;
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

{ TSettingBybitFrame }

constructor TSettingBybitFrame.Create(AOwner: TComponent);
begin
  inherited;
  SettingTactics := TSettingTacticsFrame.Create(nil);
  SettingTactics.Parent := LayoutTactics;
  SettingTactics.Align := TAlignLayout.Client;
end;

destructor TSettingBybitFrame.Destroy;
begin
  FreeAndNil(SettingTactics);
  inherited;
end;

procedure TSettingBybitFrame.Load;
begin
  EditSymble.Text    := ParamApplication.Symble;
  EditApiKey.Text    := ParamApplication.ApiKey;
  EditApiSecret.Text := ParamApplication.ApiSecret;
  SettingTactics.Load;
end;

procedure TSettingBybitFrame.Save;
begin
  ParamApplication.Symble    := EditSymble.Text;
  ParamApplication.ApiKey    := EditApiKey.Text;
  ParamApplication.ApiSecret := EditApiSecret.Text;
  SettingTactics.Save;
end;

end.
