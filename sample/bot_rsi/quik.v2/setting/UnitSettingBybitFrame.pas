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
  FMX.Objects;

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
  {todo: загрузить параметры}
end;

procedure TSettingBybitFrame.Save;
begin
  {todo: сохранить параметры}
end;

end.
