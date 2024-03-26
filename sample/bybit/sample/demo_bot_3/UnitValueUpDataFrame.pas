unit UnitValueUpDataFrame;

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
  FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.Edit;

type
  TValueUpDataFrame = class(TFrame)
    Rectangle: TRectangle;
    RectanglePlus: TRectangle;
    RectangleMinus: TRectangle;
    ButtonPlus: TButton;
    ButtonMinus: TButton;
    TextTitle: TText;
    EditValue: TEdit;
    GridLayout: TGridPanelLayout;
    GridLayoutButtom: TGridPanelLayout;
    procedure ButtonPlusClick(Sender: TObject);
    procedure ButtonMinusClick(Sender: TObject);
  private
    FStep: Double;
    FValue: Double;
    FValueMin: Double;
    FValueMax: Double;
    procedure SetValue(const Value: Double);
    function GetTitle: String;
    procedure SetTitle(const Value: String);
    procedure SetStep(const Value: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Step: Double  read FStep write SetStep;
    property Title: String read GetTitle write SetTitle;
    property Value: Double read FValue write SetValue;
    property ValueMin: Double read FValueMin write FValueMin;
    property ValueMax: Double read FValueMax write FValueMax;
  end;

implementation

{$R *.fmx}

{ TValueUpDataFrame }

constructor TValueUpDataFrame.Create(AOwner: TComponent);
begin
  inherited;
  FValueMin := 0;
  FValueMax := 100;
end;

destructor TValueUpDataFrame.Destroy;
begin

  inherited;
end;

function TValueUpDataFrame.GetTitle: String;
begin
  Result := TextTitle.Text;
end;

procedure TValueUpDataFrame.SetStep(const Value: Double);
begin
  FStep := Value;
end;

procedure TValueUpDataFrame.SetTitle(const Value: String);
begin
  TextTitle.Text := Value;
end;

procedure TValueUpDataFrame.SetValue(const Value: Double);
begin
  FValue := Value;
  EditValue.Text := FloatToStr(FValue);
end;

procedure TValueUpDataFrame.ButtonMinusClick(Sender: TObject);
begin
  FValue := StrToFloatDef(EditValue.Text,0);
  FValue := FValue - FStep;
  if FValue < FValueMin then
    FValue := FValueMin;
  EditValue.Text := FloatToStr(FValue);
end;

procedure TValueUpDataFrame.ButtonPlusClick(Sender: TObject);
begin
  FValue := StrToFloatDef(EditValue.Text,0);
  FValue := FValue + FStep;
  if FValue > FValueMax then
    FValue := FValueMax;
  EditValue.Text := FloatToStr(FValue);
end;

end.
