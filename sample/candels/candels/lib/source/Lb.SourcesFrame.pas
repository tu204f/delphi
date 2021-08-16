unit Lb.SourcesFrame;

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
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  Lb.Candel.Source;

type
  TSourcesFrame = class(TFrame)
    StringGrid: TStringGrid;
    StringColumnDate: TStringColumn;
    StringColumnTime: TStringColumn;
    StringColumnOpen: TStringColumn;
    StringColumnHigh: TStringColumn;
    StringColumnLow: TStringColumn;
    StringColumnClose: TStringColumn;
    StringColumnVol: TStringColumn;
  private
    { Private declarations }
  public
    procedure SetSource(const ASources: TSourceCandel);
    class function GetCreateFrame(const AParent: TFmxObject): TSourcesFrame; static;
    class procedure SetFreeAndNil; static;
  end;

implementation

{$R *.fmx}

var
  localSourcesFrame: TSourcesFrame = nil;

{ TSourceFrame }

class function TSourcesFrame.GetCreateFrame(const AParent: TFmxObject): TSourcesFrame;
begin
  if not Assigned(localSourcesFrame) then
  begin
    localSourcesFrame := TSourcesFrame.Create(nil);
  end;
  localSourcesFrame.Parent := AParent;
  localSourcesFrame.Align := TAlignLayout.Client;
  Result := localSourcesFrame;
end;

class procedure TSourcesFrame.SetFreeAndNil;
begin
  if Assigned(localSourcesFrame) then
    FreeAndNil(localSourcesFrame);
  localSourcesFrame := nil;
end;

procedure TSourcesFrame.SetSource(const ASources: TSourceCandel);
begin
  StringGrid.RowCount := ASources.Candels.Count;
  for var i := 0 to ASources.Candels.Count - 1 do
  begin
    var xCandel := ASources.Candels[i];
    with StringGrid do
    begin
      Cells[0,i] := DateToStr(xCandel.Date);
      Cells[1,i] := TimeToStr(xCandel.Time);
      Cells[2,i] := FloatToStr(xCandel.Open);
      Cells[3,i] := FloatToStr(xCandel.High);
      Cells[4,i] := FloatToStr(xCandel.Low);
      Cells[5,i] := FloatToStr(xCandel.Close);
      Cells[6,i] := FloatToStr(xCandel.Vol);
    end;
  end;
end;

end.
