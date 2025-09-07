program OverData;

type
  BookRec = record
    Title: string;
    Year: Integer;
  end;

overlay procedure GetBook(I: Integer; var Book: BookRec);
const
  Library: array[1..12] of BookRec = (
    (Title: 'A Scandal in Bohemia'; Year: 1891),
    (Title: 'The Red-Headed League'; Year: 1891),
    (Title: 'A Case of Identity'; Year: 1891),
    (Title: 'The Boscombe Valley Mystery'; Year: 1891),
    (Title: 'The Five Orange Pips'; Year: 1891),
    (Title: 'The Man with the Twisted Lip'; Year: 1891),
    (Title: 'The Adventure of the Blue Carbuncle'; Year: 1892),
    (Title: 'The Adventure of the Speckled Band'; Year: 1892),
    (Title: 'The Adventure of the Engineer''s Thumb'; Year: 1892),
    (Title: 'The Adventure of the Noble Bachelor'; Year: 1982),
    (Title: 'The Adventure of the Beryl Coronet'; Year: 1892),
    (Title: 'The Adventure of the Copper Beeches'; Year: 1892)
  );
begin
  Book := Library[I];
end;

var
  I: Integer;
  Book: BookRec;

begin
  ClrScr;
  for I := 1 to 12 do
  begin
    GetBook(I, Book);
    WriteLn(Book.Title, ' (', Book.Year, ')');
  end;
end.