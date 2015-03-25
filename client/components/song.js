import React from 'react';

import SongMeta from './song-meta';
import SongContent from './song-content';
import SongTextSize from './song-text-size';
import SongShowDuplicatedChoruses from './song-show-duplicated-choruses';
import Chords from './chords';

export default React.createClass({

    render: function () {
        return (
            <main>
                <SongTextSize
                    onIncrement={this.props.onIncrementFontSize}
                    onDecrement={this.props.onDecrementFontSize} />
                <SongShowDuplicatedChoruses
                    onToggleShowDuplicatedChoruses={this.props.onToggleShowDuplicatedChoruses}
                    showDuplicatedChoruses={this.props.showDuplicatedChoruses} />
                <SongMeta data={this.props.data.meta} />
                <Chords data={this.props.data.chords} />
                <SongContent data={this.props.data.content}
                             showDuplicatedChoruses={this.props.showDuplicatedChoruses}
                             fontSize={this.props.fontSize} />
            </main>
        );
    }

});
