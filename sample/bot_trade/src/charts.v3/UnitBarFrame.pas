unit UnitBarFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  Lb.SysUtils.Candel;

type
  ///<summary>Одна свеча</summary>
  TBarFrame = class(TFrame)
    Layout: TLayout;
    LineBody: TLine;
    LineOpen: TLine;
    LineClose: TLine;
    Rectangle1: TRectangle;
  private
    FMaxValue: Double;
    FMinValue: Double;
    FCandel: TCandel;
    function GetColorBody: TAlphaColor;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShowCandel;
    procedure SetCandel(const AMaxValue, AMinValue: Double; const ACandel: TCandel);
  end;

  ///<summary>Список свечей</summary>
  TBarFrameList = TObjectList<TBarFrame>;

implementation

{$R *.fmx}

function GetMaxValue(const AValue1, AValue2: Double): Double;
begin
  if AValue1 > AValue2 then
    Result := AValue1
  else
    Result := AValue2;
end;

function GetMinValue(const AValue1, AValue2: Double): Double;
begin
  if AValue1 < AValue2 then
    Result := AValue1
  else
    Result := AValue2;
end;

{ TBarFrame }

constructor TBarFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TBarFrame.Destroy;
begin

  inherited;
end;

procedure TBarFrame.Loaded;
begin
  inherited Loaded;
  Rectangle1.Align := TAlignLayout.Client;
end;

function TBarFrame.GetColorBody: TAlphaColor;
begin
  if FCandel.Open < FCandel.Close then
    Result := TAlphaColorRec.Green
  else if FCandel.Open > FCandel.Close then
    Result := TAlphaColorRec.Red
  else
    Result := TAlphaColorRec.Black
end;

procedure TBarFrame.SetCandel(const AMaxValue, AMinValue: Double;
  const ACandel: TCandel);
begin
  FMaxValue := AMaxValue;
  FMinValue := AMinValue;
  FCandel   := ACandel;
end;


procedure TBarFrame.SetShowCandel;

  function GetPositionY(const APrice, AMaxValue, AMinValue: Double; const AHieght: Single): Single;
  begin
    Result := 0;
    if (AMaxValue <> AMinValue) then
      Result := AHieght * ((AMaxValue - APrice)/(AMaxValue - AMinValue));
  end;

  procedure _SetShowCandelImage;
  var
    xOpen, xHigh, xLow, xClose: Single;
    X, Y, xWidth, xHeight, xDeltaWidth: Single;
  begin
    xOpen  := GetPositionY(FCandel.Open,  FMaxValue, FMinValue, Self.Height);
    xHigh  := GetPositionY(FCandel.High,  FMaxValue, FMinValue, Self.Height);
    xLow   := GetPositionY(FCandel.Low,   FMaxValue, FMinValue, Self.Height);
    xClose := GetPositionY(FCandel.Close, FMaxValue, FMinValue, Self.Height);


    xDeltaWidth := Rectangle1.Width/2; // - (LineBody.Stroke.Thickness/2);

    case FCandel.Status of
      tcSource: Rectangle1.Fill.Color := TAlphaColorRec.Null;
      tcFuture: Rectangle1.Fill.Color := TAlphaColorRec.Lime;
    end;


    // Тело бара
    X := xDeltaWidth;
    Y := xHigh;
    xHeight := xLow - xHigh;
    xWidth := LineBody.Stroke.Thickness;
    LineBody.SetBounds(X, Y, xWidth, xHeight);

    // Открытие
    X := 0;
    Y := xOpen;
    xHeight := LineBody.Stroke.Thickness;
    xWidth := xDeltaWidth + LineBody.Stroke.Thickness;
    LineOpen.SetBounds(X, Y, xWidth, xHeight);


    // Открытие
    X := xDeltaWidth - LineBody.Stroke.Thickness/2;
    Y := xClose;
    xHeight := LineBody.Stroke.Thickness;
    xWidth := xDeltaWidth + LineBody.Stroke.Thickness;
    LineClose.SetBounds(X, Y, xWidth, xHeight);

    LineBody.Stroke.Color := GetColorBody;
    LineOpen.Stroke.Color := GetColorBody;
    LineClose.Stroke.Color := GetColorBody;
  end;

begin
  _SetShowCandelImage;
end;

end.
