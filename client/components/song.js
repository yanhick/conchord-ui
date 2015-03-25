import React from 'react';

import SongMeta from './song-meta';
import SongContent from './song-content';
import SongTextSize from './song-text-size';

export default React.createClass({

    render: function () {
        return (
            <main>
                <SongTextSize
                    onIncrement={this.props.onIncrementFontSize}
                    onDecrement={this.props.onDecrementFontSize} />
                <SongMeta data={this.props.data.meta} />
                <SongContent data={this.props.data.content}
                             fontSize={this.props.fontSize} />
            </main>
        );
    }

});
