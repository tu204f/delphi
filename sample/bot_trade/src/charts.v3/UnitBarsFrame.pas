unit UnitBarsFrame;

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
  UnitBarFrame,
  Lb.SysUtils.Candel;

type
  ///<summary>���������� ��� �������</summary>
  TBarsFrame = class(TFrame)
    Timer: TTimer;
    Rectangle: TRectangle;
    procedure TimerTimer(Sender: TObject);
  public const
    COUNT_SOURCE = 10;
    COUNT_FUTURE = 10;
  private
    FBarFrames: TBarFrameList;
    procedure SetCreateBarFrames;
    function GetCountBars: Integer;
  private
    FMaxValue: Double;
    FMinValue: Double;
    FStructure: TStructure;
    procedure SetMaxAndMinValue(var AMaxValue, AMinValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShowStructure(const AStructure: TStructure);
  end;

implementation

{$R *.fmx}

uses
  UnitMainForm;

{ TBarsFrame }

constructor TBarsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStructure := nil;
  FBarFrames := TBarFrameList.Create;
end;

destructor TBarsFrame.Destroy;
begin
  FreeAndNil(FBarFrames);
  inherited;
end;

function TBarsFrame.GetCountBars: Integer;
begin
  if Assigned(FStructure) then
  begin
    Result := FStructure.SourceVectors.Count +
              FStructure.FutureVectors.Count;
  end
  else
    Result := COUNT_SOURCE + COUNT_FUTURE;
end;

procedure TBarsFrame.SetCreateBarFrames;
var
  i: Integer;
  xDelta: Single;
  xBar: TBarFrame;
  xX, xY, xWidth, xHeight: Single;
  iCountBars: Integer;
begin
  iCountBars := FBarFrames.Count;
  if iCountBars > 0 then
  begin
    xDelta := (Self.Width - 2)/iCountBars;
    for i := 0 to iCountBars - 1 do
    begin
      xBar := FBarFrames[i];

      if i = 0 then
        xX := xDelta * i + 1
      else
        xX := xDelta * i;

      xY := 0;
      xWidth := xDelta;
      xHeight := Self.Height;

      xBar.SetBounds(xX,xY,xWidth,xHeight);
      xBar.SetShowCandel;

    end;
  end;
end;

procedure TBarsFrame.TimerTimer(Sender: TObject);
begin
  SetCreateBarFrames;
end;

procedure TBarsFrame.SetMaxAndMinValue(var AMaxValue, AMinValue: Double);

  procedure _SetMaxAndMin(const ACandels: TCandelList; var AMaxValue, AMinValue: Double);
  var
    i, iCount: Integer;
    xCandel: TCandel;
  begin
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      xCandel := ACandels.Items[0];

      if AMaxValue = 0 then
         AMaxValue := xCandel.High
      else
        if xCandel.High > AMaxValue then
          AMaxValue := xCandel.High;

      if AMinValue = 0 then
        AMinValue := xCandel.Low
      else
        if xCandel.Low  < AMinValue then
          AMinValue := xCandel.Low;

      for i := 1 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.High > AMaxValue then
          AMaxValue := xCandel.High;
        if xCandel.Low  < AMinValue then
          AMinValue := xCandel.Low;
      end;
    end;
  end;

var
  i, iCount: Integer;
  xCandel: TCandel;
begin
  AMaxValue := 0;
  AMinValue := 0;

  iCount := FStructure.SourceVectors.Count;
  if iCount > 0 then
  begin
    xCandel := FStructure.SourceVectors.Items[0];
    AMaxValue := xCandel.High;
    AMinValue := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin
      xCandel := FStructure.SourceVectors[i];
      if xCandel.High > AMaxValue then AMaxValue := xCandel.High;
      if xCandel.Low  < AMinValue then AMinValue := xCandel.Low;
    end;
  end;

  iCount := FStructure.FutureVectors.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      xCandel := FStructure.FutureVectors[i];
      if xCandel.High > AMaxValue then AMaxValue := xCandel.High;
      if xCandel.Low  < AMinValue then AMinValue := xCandel.Low;
    end;
  end;

end;

procedure TBarsFrame.SetShowStructure(const AStructure: TStructure);
var
  i, iCount: Integer;
  xBar: TBarFrame;
  xCandel: TCandel;
begin
  FMaxValue := 0;
  FMinValue := 0;

  FStructure := AStructure;
  FBarFrames.Clear;
  SetMaxAndMinValue(FMaxValue,FMinValue);

  iCount := FStructure.SourceVectors.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xBar := TBarFrame.Create(nil);
      xBar.Parent := Rectangle;
      xCandel := FStructure.SourceVectors.Items[i];
      xBar.SetCandel(FMaxValue,FMinValue,xCandel);
      FBarFrames.Add(xBar);
    end;


  iCount := FStructure.FutureVectors.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xBar := TBarFrame.Create(nil);
      xBar.Parent := Rectangle;
      xCandel := FStructure.FutureVectors.Items[i];
      xCandel.Status := TTypeCandel.tcFuture;
      xBar.SetCandel(FMaxValue,FMinValue,xCandel);
      FBarFrames.Add(xBar);
    end;

end;

end.