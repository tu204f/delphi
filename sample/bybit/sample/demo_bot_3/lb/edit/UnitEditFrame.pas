unit UnitEditFrame;

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
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.Layouts;

type
  TValueFrame = class(TFrame)
    EditLayout: TLayout;
    ButtonPlus: TButton;
    ButtonMinus: TButton;
    EditQuantity: TEdit;
    procedure ButtonPlusClick(Sender: TObject);
    procedure ButtonMinusClick(Sender: TObject);
    procedure EditQuantityChange(Sender: TObject);
  public type
    TTypeValue = (tvUp, tvDown);
  private
    FValue: Double;
    FStep: Double;
    FMaxValue, FMinValue: Double;
    procedure SetValueEdit(const AStatus: TTypeValue);
    procedure SetValue(const Value: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Value: Double read FValue write SetValue;
    property Step: Double read FStep write FStep;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property MinValue: Double read FMinValue write FMinValue;
  end;

implementation

{$R *.fmx}

{ TValueFrame }

constructor TValueFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TValueFrame.Destroy;
begin

  inherited;
end;

procedure TValueFrame.ButtonPlusClick(Sender: TObject);
begin
  SetValueEdit(TTypeValue.tvUp);
end;

procedure TValueFrame.ButtonMinusClick(Sender: TObject);
begin
  SetValueEdit(TTypeValue.tvDown);
end;

procedure TValueFrame.SetValue(const Value: Double);
var
  xF: TFormatSettings;
begin
  FValue := Value;
  EditQuantity.BeginUpdate;
  try
    xF := FormatSettings;
    xF.DecimalSeparator := '.';
    EditQuantity.Text := FloatToStr(FValue,xF);
  finally
    EditQuantity.EndUpdate;
  end;
end;

procedure TValueFrame.EditQuantityChange(Sender: TObject);
var
  xS: String;
  xF: TFormatSettings;
begin
  if Sender is TEdit then
  begin
    xF := FormatSettings;
    xF.DecimalSeparator := '.';
    xS := TEdit(Sender).Text;
    FValue := StrToFloatDef(xS,0,xF);
  end;
end;

procedure TValueFrame.SetValueEdit(const AStatus: TTypeValue);
var
  xF: TFormatSettings;
begin
  case AStatus of
    tvUp: FValue := FValue + FStep;
    tvDown: FValue := FValue - FStep;
  end;

  if FValue <= 0 then
    FValue := FStep;

  if FValue > FMaxValue then
    FValue := FMaxValue;

  if FValue < FMinValue then
    FValue := FMinValue;

  EditQuantity.BeginUpdate;
  try
    xF := FormatSettings;
    xF.DecimalSeparator := '.';
    EditQuantity.Text := FloatToStr(FValue,xF);
  finally
    EditQuantity.EndUpdate;
  end;
end;

end.
