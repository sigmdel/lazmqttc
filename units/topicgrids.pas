unit topicgrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Grids, brokerunit;

type
  
  { TSubTopicsGrid }

  TSubTopicsGrid = class(TCustomDrawGrid)
  protected
    FBroker: TBroker;
    procedure SetBroker(aBroker: TBroker);
    procedure DrawTextInCell(aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState); override;
    procedure DrawGridCheckboxBitmaps(const aCol,aRow: Integer; const aRect: TRect;
                                        const aState: TCheckboxState); override;
    function  GetEditText(aCol, aRow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure SetCheckboxState(const aCol, aRow:Integer; const aState: TCheckboxState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditorTextChanged(const aCol,aRow: Integer; const aText:string); override;
    procedure HideEditor;
    procedure UpdateGridSize;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    property Broker: TBroker read FBroker write SetBroker;
  published
    property Align;
//    property AlternateColor;
    property Anchors;
    property AutoAdvance;
    property AutoEdit;
    property AutoFillColumns;
    //property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
//    property CellHintPriority;
    property Color;
    property ColCount;
    property ColumnClickSorts;
    property Columns;
    property Constraints;
    property DefaultColWidth;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property FixedColor;
    property FixedCols;
    property FixedRows;
    property Flat;
    property Font;
    property GridLineWidth;
    property HeaderHotZones;
    property HeaderPushZones;
    property ImageIndexSortAsc;
    property ImageIndexSortDesc;
    property MouseWheelOption;
    property Options;
    property Options2;
    //property ParentBiDiMode;
    property ParentColor default false;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RangeSelectMode;
    property RowCount;
    property ScrollBars;
    property ShowHint;
    property TabAdvance;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleImageListWidth;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;

    property OnAfterSelection;
    property OnBeforeSelection;
    property OnCheckboxToggled;
    property OnClick;
    property OnColRowDeleted;
    property OnColRowExchanged;
    property OnColRowInserted;
    property OnColRowMoved;
    property OnCompareCells;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    //property OnEditButtonClick; deprecated;
    property OnButtonClick;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCellHint;
    property OnGetCheckboxState;
    property OnGetEditMask;
    property OnGetEditText;
    property OnHeaderClick;
    property OnHeaderSized;
    property OnHeaderSizing;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPickListSelect;
    property OnPrepareCanvas;
    property OnSelectEditor;
    property OnSelection;
    property OnSelectCell;
    property OnSetCheckboxState;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopleftChanged;
    property OnUserCheckboxBitmap;
    property OnUTF8KeyPress;
    property OnValidateEntry;
  end;

implementation

constructor TSubTopicsGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ExtendedSelect := False;
  FixedCols := 0;
  Flat := True;
  RowCount := 1;
  AutoAdvance := aaNone;
  Options := Options + [(*goAlwaysShowEditor,*) goEditing] - [goRangeSelect];
  ScrollBars := ssAutoVertical;
  with Columns.Add do begin
    Title.Alignment := taCenter;
    Title.Caption := 'Use';
    ButtonStyle := cbsCheckboxColumn;
  end;
  with Columns.Add do begin
    Title.Alignment := taCenter;
    Title.Caption := 'Topic';
  end;
  with Columns.Add do begin
    Title.Alignment := taCenter;
    Title.Caption := 'QoS';
    Alignment := taCenter;
    {
    PickList.Add('0 - At most once');
    PickList.Add('1 - At least once');
    PickList.Add('2 - Exactly once');
    }
    PickList.Add('0');
    PickList.Add('1');
    PickList.Add('2');
  end;
end;

procedure TSubTopicsGrid.DrawGridCheckboxBitmaps(const aCol, aRow: Integer;
  const aRect: TRect; const aState: TCheckboxState);
var
  state: TCheckboxState;
begin
  if aRow > 0 then begin
    if FBroker.SubTopics[aRow-1].Use then
      state := cbChecked
    else
      state := cbUnchecked;
    inherited DrawGridCheckboxBitmaps(aCol, aRow, aRect, state);
  end;
end;

procedure TSubTopicsGrid.DrawTextInCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  if aRow > 0 then begin
    if aCol = 1 then
      DrawCellText(aCol, aRow, aRect, aState, FBroker.SubTopics[aRow-1].topic)
    else if aCol = 2 then
      DrawCellText(aCol, aRow, aRect, aState, inttostr(FBroker.SubTopics[aRow-1].qos))
  end;
end;

procedure TSubTopicsGrid.EditorTextChanged(const aCol,aRow: Integer; const aText:string);
begin
  if (aRow > 0) and (aRow <= FBroker.SubTopicsCount) and (aCol = 2) then
    FBroker.SubTopics[pred(aRow)].QoS := TPickListCellEditor(Editor).ItemIndex;
end;

function TSubTopicsGrid.GetEditText(aCol, aRow: Longint): string;
begin
  if (aRow > 0) and (aRow <= FBroker.SubTopicsCount) and (aCol = 1) then
    result := FBroker.SubTopics[pred(aRow)].Topic
  else
    result := '';
end;

procedure TSubTopicsGrid.HideEditor;
begin
  if assigned(Editor) then
    Editor.Visible := false;
end;

procedure TSubTopicsGrid.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if ColCount > 2 then begin
    ColWidths[0] := 50;
    ColWidths[2] := 50;
    ColWidths[1] := clientwidth - 100 - 1;
  end;
end;

procedure TSubTopicsGrid.SetBroker(aBroker: TBroker);
begin
  FBroker := aBroker;
  UpdateGridSize;
end;

procedure TSubTopicsGrid.SetCheckboxState(const aCol, aRow: Integer;
  const aState: TCheckboxState);
var
  newState: TCheckboxState;
begin
  newState := aState;
  if (aRow > 0) and (aRow <= FBroker.SubTopicsCount) then begin
    FBroker.SubTopics[pred(aRow)].Use := not FBroker.SubTopics[pred(aRow)].Use;
    if FBroker.SubTopics[pred(aRow)].Use then
      newState := cbChecked
    else
      newState := cbUnchecked;
  end;
  inherited SetCheckboxState(aCol, aRow, newState);
end;

procedure TSubTopicsGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  if (aRow > 0) and (aRow <= FBroker.SubTopicsCount) and (aCol = 1) then
    FBroker.SubTopics[pred(aRow)].Topic := Value;
end;

procedure TSubTopicsGrid.UpdateGridSize;
begin
  EditorHide;
  if assigned(FBroker) then
    RowCount := FBroker.SubTopicsCount + 1
  else
    RowCount := 1;
end;

end.

