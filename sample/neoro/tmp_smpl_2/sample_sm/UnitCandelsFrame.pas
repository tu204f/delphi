unit UnitCandelsFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Grid;

type
  ///<summary>Пока не придумал зачем мне это нужно</sumamry>
  TGridFrame = class(TFrame)
    StrGrid: TStringGrid;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TGridFrame }

constructor TGridFrame.Create(AOwner: TComponent);

  procedure _AddColumn(AStrinGrid: TStringGrid; const AName: String);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(AStrinGrid);
    xColumn.Header := AName;
    xColumn.Parent := AStrinGrid;
  end;

begin
  inherited Create(AOwner);

  _AddColumn(StrGrid,'Дата');
  _AddColumn(StrGrid,'Время');
  _AddColumn(StrGrid,'Open');
  _AddColumn(StrGrid,'High');
  _AddColumn(StrGrid,'Low');
  _AddColumn(StrGrid,'Close');
  _AddColumn(StrGrid,'Vol');

end;

destructor TGridFrame.Destroy;
begin

  inherited;
end;

end.
