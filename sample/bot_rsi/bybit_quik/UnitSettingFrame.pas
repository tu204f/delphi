unit UnitSettingFrame;

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
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.Edit,
  Lb.SysUtils;

type
  TSettingFrame = class(TFrame)
    Rectangle: TRectangle;
    ButtonApply: TButton;
    ButtonClose: TButton;
    Text1: TText;
    EditSymble: TEdit;
    EditApiKey: TEdit;
    Text2: TText;
    Text3: TText;
    EditApiSecret: TEdit;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    FMainApp: IMainApp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSetting;
    procedure SaveSetting;
    property MainApp: IMainApp write FMainApp;
  end;

implementation

{$R *.fmx}

{ TSettingFrame }

constructor TSettingFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TSettingFrame.Destroy;
begin

  inherited;
end;

procedure TSettingFrame.LoadSetting;
begin
  ParamApplication.Load;
  EditSymble.Text    := ParamApplication.Symble;
  EditApiKey.Text    := ParamApplication.ApiKey;
  EditApiSecret.Text := ParamApplication.ApiSecret;
end;

procedure TSettingFrame.SaveSetting;
begin
  ParamApplication.Symble    := EditSymble.Text;
  ParamApplication.ApiKey    := EditApiKey.Text;
  ParamApplication.ApiSecret := EditApiSecret.Text;
  ParamApplication.Save;
end;

procedure TSettingFrame.ButtonApplyClick(Sender: TObject);
begin
  SaveSetting;
end;

procedure TSettingFrame.ButtonCloseClick(Sender: TObject);
begin
  FMainApp.EventCloseTabControl;
end;

end.
