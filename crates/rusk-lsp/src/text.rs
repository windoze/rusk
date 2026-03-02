use tower_lsp::lsp_types::{Position, Range};

fn compute_line_starts(src: &str) -> Vec<usize> {
    let bytes = src.as_bytes();
    let mut line_starts = vec![0];
    let mut i = 0usize;
    while i < bytes.len() {
        match bytes[i] {
            b'\n' => {
                line_starts.push(i.saturating_add(1));
                i = i.saturating_add(1);
            }
            b'\r' => {
                if bytes.get(i.saturating_add(1)) == Some(&b'\n') {
                    line_starts.push(i.saturating_add(2));
                    i = i.saturating_add(2);
                } else {
                    line_starts.push(i.saturating_add(1));
                    i = i.saturating_add(1);
                }
            }
            _ => i = i.saturating_add(1),
        }
    }
    line_starts
}

fn clamp_to_char_boundary(src: &str, mut offset: usize) -> usize {
    offset = offset.min(src.len());
    while offset > 0 && !src.is_char_boundary(offset) {
        offset = offset.saturating_sub(1);
    }
    offset
}

pub fn byte_offset_to_position(src: &str, offset: usize) -> Position {
    let offset = clamp_to_char_boundary(src, offset);
    let line_starts = compute_line_starts(src);

    let line_idx = line_starts
        .partition_point(|&start| start <= offset)
        .saturating_sub(1);
    let line_start = *line_starts.get(line_idx).unwrap_or(&0);

    let utf16_col: usize = src[line_start..offset]
        .chars()
        .map(|ch| ch.len_utf16())
        .sum();

    Position {
        line: line_idx as u32,
        character: utf16_col as u32,
    }
}

pub fn position_to_byte_offset(src: &str, position: Position) -> usize {
    let line_starts = compute_line_starts(src);
    let line = position.line as usize;
    if line_starts.is_empty() {
        return 0;
    }
    if line >= line_starts.len() {
        return src.len();
    }

    let line_start = line_starts[line];
    let mut line_end = if line.saturating_add(1) < line_starts.len() {
        line_starts[line + 1]
    } else {
        src.len()
    };

    // Exclude trailing newline from the editable line slice.
    if line_end > line_start {
        let bytes = src.as_bytes();
        if bytes.get(line_end.saturating_sub(1)) == Some(&b'\n') {
            line_end = line_end.saturating_sub(1);
            if bytes.get(line_end.saturating_sub(1)) == Some(&b'\r') && line_end > line_start {
                line_end = line_end.saturating_sub(1);
            }
        } else if bytes.get(line_end.saturating_sub(1)) == Some(&b'\r') {
            line_end = line_end.saturating_sub(1);
        }
    }

    let target = position.character as usize;
    let mut utf16_count = 0usize;

    for (idx, ch) in src[line_start..line_end].char_indices() {
        let next = utf16_count.saturating_add(ch.len_utf16());
        if next > target {
            // Target falls in the middle of this UTF-16 sequence (surrogate pair). Clamp to the
            // start of the character.
            return line_start.saturating_add(idx);
        }
        if next == target {
            return line_start.saturating_add(idx).saturating_add(ch.len_utf8());
        }
        utf16_count = next;
    }

    line_end
}

pub fn byte_range_to_range(src: &str, start: usize, end: usize) -> Range {
    let start = start.min(src.len());
    let end = end.min(src.len());
    Range {
        start: byte_offset_to_position(src, start),
        end: byte_offset_to_position(src, end),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn utf16_positions_handle_surrogate_pairs() {
        // 😀 is U+1F600, which is 2 UTF-16 code units.
        let src = "😀a";

        assert_eq!(
            byte_offset_to_position(src, 0),
            Position {
                line: 0,
                character: 0
            }
        );
        assert_eq!(
            byte_offset_to_position(src, "😀".len()),
            Position {
                line: 0,
                character: 2
            }
        );
        assert_eq!(
            byte_offset_to_position(src, "😀a".len()),
            Position {
                line: 0,
                character: 3
            }
        );

        assert_eq!(
            position_to_byte_offset(
                src,
                Position {
                    line: 0,
                    character: 0
                }
            ),
            0
        );
        assert_eq!(
            position_to_byte_offset(
                src,
                Position {
                    line: 0,
                    character: 1
                }
            ),
            0,
            "a UTF-16 column in the middle of a surrogate pair clamps to the start of the char"
        );
        assert_eq!(
            position_to_byte_offset(
                src,
                Position {
                    line: 0,
                    character: 2
                }
            ),
            "😀".len()
        );
        assert_eq!(
            position_to_byte_offset(
                src,
                Position {
                    line: 0,
                    character: 3
                }
            ),
            "😀a".len()
        );
    }

    #[test]
    fn position_to_byte_offset_clamps_past_end_of_line() {
        let src = "a\nb";
        assert_eq!(
            position_to_byte_offset(
                src,
                Position {
                    line: 0,
                    character: 10
                }
            ),
            1
        );
        assert_eq!(
            position_to_byte_offset(
                src,
                Position {
                    line: 1,
                    character: 10
                }
            ),
            3
        );
    }
}
