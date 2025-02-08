unit UnitBarFrame;

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
  FMX.Layouts;

type
  ///<summary>Единица свечи или бар</summary>
  TBarFrame = class(TFrame)
    RectangleFon: TRectangle;
    LayoutTop: TLayout;
    LayoutBottom: TLayout;
    GridPanelLayoutHigh: TGridPanelLayout;
    GridPanelLayoutLow: TGridPanelLayout;
    LineHigh: TLine;
    Layout1: TLayout;
    LineLow: TLine;
    Layout2: TLayout;
    RectangleBody: TRectangle;
  private
    FID: Integer;
    FOpenY, FHighY, FLowY, FCloseY: Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBoundsCandel(const AOpen, AHigh, ALow, AClose: Single);
    property ID: Integer read FID write FID;
  end;

implementation

{$R *.fmx}

{ TBarFrame }

constructor TBarFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TBarFrame.Destroy;
begin
  inherited;
end;

procedure TBarFrame.SetBoundsCandel(const AOpen, AHigh, ALow, AClose: Single);

  function _Max(const AValue1, AValue2: Single): Single;
  begin
    if AValue1 > AValue2 then
      Result := AValue1
    else
      Result := AValue2;
  end;

  function _Min(const AValue1, AValue2: Single): Single;
  begin
    if AValue1 < AValue2 then
      Result := AValue1
    else
      Result := AValue2;
  end;

begin

  if AOpen < AClose then
    RectangleBody.Fill.Color := TAlphaColorRec.Red
  else if AOpen > AClose then
    RectangleBody.Fill.Color := TAlphaColorRec.Green
  else
    RectangleBody.Fill.Color := TAlphaColorRec.Black;

  FOpenY  := _Max(AOpen,AClose);
  FHighY  := AHigh;
  FLowY   := ALow;
  FCloseY := _Min(AOpen,AClose);

  LayoutTop.Height := FHighY;
  LayoutBottom.Height := (RectangleFon.Height - FLowY);
  GridPanelLayoutHigh.Height := (FCloseY - FHighY);
  GridPanelLayoutLow.Height := (FLowY - FOpenY);
end;

end.
