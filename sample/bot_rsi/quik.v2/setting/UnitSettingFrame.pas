unit UnitSettingFrame;

interface

{$i platform.inc}

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
{$IFDEF BYBIT}
  UnitSettingBybitFrame,
{$ENDIF}
{$IFDEF QUIK}
  UnitSettingQuikFrame,
{$ENDIF}
  Lb.SysUtils;

type
  TSettingFrame = class(TFrame)
    Rectangle: TRectangle;
    ButtonApply: TButton;
    ButtonClose: TButton;
    Layout: TLayout;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
  {$IFDEF BYBIT}
    FSettingBybit: TSettingBybitFrame;
  {$ENDIF}
  {$IFDEF QUIK}
    FSettingQuik : TSettingQuikFrame;
  {$ENDIF}
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

  function _CreateFrame: TFrame;
  begin
  {$IFDEF BYBIT}
    FSettingBybit := TSettingBybitFrame.Create(nil);
    Result := FSettingBybit;
  {$ENDIF}
  {$IFDEF QUIK}
    FSettingQuik  := TSettingQuikFrame.Create(nil);
    Result := FSettingQuik;
  {$ENDIF}
  end;

var
  xFrame: TFrame;
begin
  inherited Create(AOwner);
  xFrame := _CreateFrame;
  xFrame.Parent := Layout;
  xFrame.Align := TAlignLayout.Client;
end;

destructor TSettingFrame.Destroy;
begin

  inherited;
end;

procedure TSettingFrame.LoadSetting;
begin
  ParamApplication.Load;
  {$IFDEF BYBIT}
  FSettingBybit.Load;
  {$ENDIF}
  {$IFDEF QUIK}
  FSettingQuik.Load;
  {$ENDIF}
end;

procedure TSettingFrame.SaveSetting;
begin
  {$IFDEF BYBIT}
  FSettingBybit.Save;
  {$ENDIF}
  {$IFDEF QUIK}
  FSettingQuik.Save;
  {$ENDIF}
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
