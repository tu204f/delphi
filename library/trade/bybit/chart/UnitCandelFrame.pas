unit UnitCandelFrame;

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
  Lb.SysUtils.Candel,
  FMX.Layouts;

type
  TCandelFrame = class(TFrame)
    Line: TLine;
    Body: TRectangle;
    LineOC: TLine;
    Layout: TLayout;
  private
    FCandel: TCandel;
    FMaxPrice, FMinPrice: Double;
  protected
    FOpenY: Double;
    FHighY: Double;
    FLowY: Double;
    FCloseY: Double;
    function GetPriceToY(const APrice: Double): Double;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Build;
    property Candel: TCandel read FCandel write FCandel;
    property MaxPrice: Double read FMaxPrice write FMaxPrice;
    property MinPrice: Double read FMinPrice write FMinPrice;
  end;

function PriceToY(const AHeight, APrice, AMaxPrice, AMinPrice: Double): Double;

implementation

{$R *.fmx}

function PriceToY(const AHeight, APrice, AMaxPrice, AMinPrice: Double): Double;
var
  xDelta: Double;
begin
  xDelta := (APrice - AMinPrice)/(AMaxPrice - AMinPrice);
  Result := AHeight - xDelta * AHeight;
end;


{ TCandelFrame }

constructor TCandelFrame.Create(AOwner: TComponent);
begin
  inherited;
  Line.Visible := False;
  Body.Visible := False;
  LineOC.Visible := False;
end;

destructor TCandelFrame.Destroy;
begin

  inherited;
end;

function TCandelFrame.GetPriceToY(const APrice: Double): Double;
begin
  Result := PriceToY(Layout.Height,APrice,FMaxPrice,FMinPrice);
end;

procedure TCandelFrame.Build;

  function _GetTopY: Double;
  begin
    case FCandel.CandelStatus of
      TCandelStatus.csGrren: Result := FCloseY;
      TCandelStatus.csRed: Result := FOpenY;
    else
      Result := FOpenY;
    end;
  end;

  function _GetBottomY: Double;
  begin
    case FCandel.CandelStatus of
      TCandelStatus.csGrren: Result := FOpenY;
      TCandelStatus.csRed: Result := FCloseY;
    else
      Result := FOpenY;
    end;
  end;

  procedure _BodyPosition;
  var
    xTop, xHeight: Double;
  begin
    xTop := _GetTopY;
    xHeight := _GetBottomY - xTop;
    // Показываем тело связи или линию


    Body.Visible :=
      (xHeight > 2) and
      (not (FCandel.CandelStatus = TCandelStatus.csNull));
    LineOC.Visible := not Body.Visible;



    if Body.Visible then
    begin
      Body.SetBounds(0,xTop,Layout.Width,xHeight);
    end
    else
    begin
      LineOC.SetBounds(0,xTop,Layout.Width,2)
    end;
  end;

  procedure _LinePosition;
  var
    xX: Double;
    xHeight: Double;
  begin
    Line.Visible := True;
    xHeight := FLowY - FHighY;
    Line.Stroke.Thickness := 2;
    xX := Layout.Width / 2 - Line.Width/2;
    Line.SetBounds(
      xX,
      FHighY,
      2,
      xHeight
    );
  end;

begin
  // Определяем координаты цен
  FOpenY  := GetPriceToY(FCandel.Open);
  FHighY  := GetPriceToY(FCandel.High);
  FLowY   := GetPriceToY(FCandel.Low);
  FCloseY := GetPriceToY(FCandel.Close);
  // Формирование объекта - тебя
  _BodyPosition;
  _LinePosition;

  case FCandel.CandelStatus of
    TCandelStatus.csGrren: Body.Fill.Color := TAlphaColorRec.Green;
    TCandelStatus.csRed  : Body.Fill.Color := TAlphaColorRec.Red;
  end;

end;

end.
