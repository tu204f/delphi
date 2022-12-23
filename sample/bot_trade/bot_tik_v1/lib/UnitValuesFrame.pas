unit UnitValuesFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox,
  UnitValueFrame;

type
  ///<summary>Цена и объем</summary>
  TValueItem = class(TListBoxItem)
  private
    FValueFrame: TValueFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ValueFrame: TValueFrame read FValueFrame;
  end;

  ///<summary>Список цены</summary>
  TValuesFrame = class(TFrame)
    ListBox: TListBox;
    procedure FrameResize(Sender: TObject);
    procedure FrameResized(Sender: TObject);
  public const
    COUNT_ITEM = 15;
  private
    procedure SetInitialization;
    procedure SetItemResize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TValueItem }

constructor TValueItem.Create(AOwner: TComponent);
begin
  inherited;
  FValueFrame := TValueFrame.Create(Self);
  FValueFrame.Parent := Self;
  FValueFrame.Align := TAlignLayout.Client;
end;

destructor TValueItem.Destroy;
begin
  FreeAndNil(FValueFrame);
  inherited;
end;

{ TValuesFrame }

constructor TValuesFrame.Create(AOwner: TComponent);
begin
  inherited;
  SetInitialization;
end;

destructor TValuesFrame.Destroy;
begin
  ListBox.Clear;
  inherited;
end;

procedure TValuesFrame.FrameResize(Sender: TObject);
begin
  SetItemResize;
end;

procedure TValuesFrame.FrameResized(Sender: TObject);
begin
  SetItemResize;
end;

procedure TValuesFrame.SetInitialization;
var
  xItem: TValueItem;
  xCount: Integer;
  xHeight: Single;
begin
  xCount := 2 * COUNT_ITEM;
  while ListBox.Count <= xCount do
  begin
    xItem := TValueItem.Create(nil);
    xItem.Parent := ListBox;
  end;
end;

procedure TValuesFrame.SetItemResize;
var
  xItem: TListBoxItem;
  i, iCount: Integer;
  xHeight: Single;
begin
  iCount := ListBox.Count;
  if iCount > 0 then
  begin
    xHeight := (ListBox.ClientHeight - 5)/iCount;
    for i := 0 to iCount - 1 do
    begin
      xItem := ListBox.ItemByIndex(i);
      xItem.Height := xHeight;
    end;
  end;
end;

end.
