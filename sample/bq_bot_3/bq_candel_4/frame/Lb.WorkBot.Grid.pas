unit Lb.WorkBot.Grid;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid;

procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);

implementation

procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
var
  xCol: TStringColumn;
begin
  xCol := TStringColumn.Create(nil);
  xCol.Parent := AStrGrid;
  xCol.Header := AHeader;
  xCol.Width  := AWidth;
end;

end.
